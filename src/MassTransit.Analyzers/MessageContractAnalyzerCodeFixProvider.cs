using System.Collections.Generic;
using System.Collections.Immutable;
using System.Composition;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Formatting;
using Microsoft.CodeAnalysis.Simplification;

namespace MassTransit.Analyzers.MessageContractAnalyzer
{
    [ExportCodeFixProvider(LanguageNames.CSharp, Name = nameof(MessageContractAnalyzerCodeFixProvider)), Shared]
    public class MessageContractAnalyzerCodeFixProvider : CodeFixProvider
    {
        private const string Title = "Add missing properties";

        public sealed override ImmutableArray<string> FixableDiagnosticIds => ImmutableArray.Create(MessageContractAnalyzerAnalyzer.MissingPropertiesRuleId);

        public sealed override FixAllProvider GetFixAllProvider()
        {
            // See https://github.com/dotnet/roslyn/blob/master/docs/analyzers/FixAllProvider.md for more information on Fix All Providers
            return WellKnownFixAllProviders.BatchFixer;
        }

        public sealed override async Task RegisterCodeFixesAsync(CodeFixContext context)
        {
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);

            // TODO: Replace the following code with your own analysis, generating a CodeAction for each fix to suggest
            var diagnostic = context.Diagnostics.First();
            var diagnosticSpan = diagnostic.Location.SourceSpan;

            // Find the type declaration identified by the diagnostic.
            var anonymousObject = root.FindToken(diagnosticSpan.Start).Parent.AncestorsAndSelf().OfType<AnonymousObjectCreationExpressionSyntax>().First();

            // Register a code action that will invoke the fix.
            context.RegisterCodeFix(
                CodeAction.Create(
                    title: Title,
                    createChangedDocument: cancellationToken => AddMissingProperties(context.Document, anonymousObject, cancellationToken),
                    equivalenceKey: Title),
                diagnostic);
        }

        private static async Task<Document> AddMissingProperties(Document document,
            AnonymousObjectCreationExpressionSyntax anonymousObject, CancellationToken cancellationToken)
        {
            var root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);
            var semanticModel = await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);

            var argumentSyntax = anonymousObject.Parent as ArgumentSyntax;
            if (argumentSyntax == null)
            {
                if (anonymousObject.Parent is InitializerExpressionSyntax initializerExpressionSyntax &&
                    initializerExpressionSyntax.Parent is ImplicitArrayCreationExpressionSyntax implicitArrayCreationExpressionSyntax)
                {
                    argumentSyntax = implicitArrayCreationExpressionSyntax.Parent as ArgumentSyntax;
                }
            }

            if (IsActivator(argumentSyntax, semanticModel, out var typeArgument) &&
                HasMessageContract(typeArgument, semanticModel, out var messageContractType))
            {
                var dictionary = new Dictionary<AnonymousObjectCreationExpressionSyntax, ITypeSymbol>();
                FindAnonymousTypesWithMessageContractsInTree(dictionary, anonymousObject, messageContractType);
                var newRoot = AddMissingProperties(root, dictionary);
                var formattedRoot = Formatter.Format(newRoot, Formatter.Annotation, document.Project.Solution.Workspace, document.Project.Solution.Workspace.Options);
                return document.WithSyntaxRoot(formattedRoot);
            }

            return document;
        }

        private static void FindAnonymousTypesWithMessageContractsInTree(IDictionary<AnonymousObjectCreationExpressionSyntax, ITypeSymbol> dictionary,
            AnonymousObjectCreationExpressionSyntax anonymousObject, ITypeSymbol messageContractType)
        {
            var messageContractProperties = GetMessageContractProperties(messageContractType);

            foreach (var initializer in anonymousObject.Initializers)
            {
                var name = GetName(initializer);

                var messageContractProperty = messageContractProperties.FirstOrDefault(p => p.Name == name);

                if (messageContractProperty != null)
                {
                    if (initializer.Expression is ImplicitArrayCreationExpressionSyntax implicitArrayCreationExpressionSyntax)
                    {
                        if (IsImmutableArray(messageContractProperty.Type, out var messageContractPropertyTypeArgument) ||
                            IsReadOnlyList(messageContractProperty.Type, out messageContractPropertyTypeArgument))
                        {
                            var expressions = implicitArrayCreationExpressionSyntax.Initializer.Expressions;
                            foreach (var expressionSyntax in expressions)
                            {
                                if (expressionSyntax is AnonymousObjectCreationExpressionSyntax anonymousObjectArrayInitializer)
                                {
                                    FindAnonymousTypesWithMessageContractsInTree(dictionary, anonymousObjectArrayInitializer, messageContractPropertyTypeArgument);
                                }
                            }
                        }
                    }
                    else if (initializer.Expression is AnonymousObjectCreationExpressionSyntax anonymousObjectProperty)
                    {
                        FindAnonymousTypesWithMessageContractsInTree(dictionary, anonymousObjectProperty, messageContractProperty.Type);
                    }
                }
            }

            dictionary.Add(anonymousObject, messageContractType);
        }

        private static string GetName(AnonymousObjectMemberDeclaratorSyntax initializer)
        {
            string name;
            if (initializer.NameEquals == null)
            {
                var expression = (MemberAccessExpressionSyntax)initializer.Expression;
                name = expression.Name.Identifier.Text;
            }
            else
            {
                name = initializer.NameEquals.Name.Identifier.Text;
            }

            return name;
        }

        private static List<IPropertySymbol> GetMessageContractProperties(ITypeSymbol messageContractType)
        {
            var messageContractTypes = new List<ITypeSymbol> { messageContractType };
            messageContractTypes.AddRange(messageContractType.AllInterfaces);

            return messageContractTypes.SelectMany(i => i.GetMembers().OfType<IPropertySymbol>()).ToList();
        }

        private static SyntaxNode AddMissingProperties(SyntaxNode root, IDictionary<AnonymousObjectCreationExpressionSyntax, ITypeSymbol> dictionary)
        {
            var newRoot = root.TrackNodes(dictionary.Keys);

            foreach (var keyValuePair in dictionary)
            {
                var anonymousObject = newRoot.GetCurrentNode(keyValuePair.Key);
                var messageContractType = keyValuePair.Value;
                newRoot = AddMissingProperties(newRoot, anonymousObject, messageContractType);
            }

            return newRoot;
        }

        private static SyntaxNode AddMissingProperties(SyntaxNode root,
            AnonymousObjectCreationExpressionSyntax anonymousObject, ITypeSymbol messageContractType)
        {
            var newRoot = root;

            var messageContractProperties = GetMessageContractProperties(messageContractType);

            var propertiesToAdd = new List<AnonymousObjectMemberDeclaratorSyntax>();
            foreach (var messageContractProperty in messageContractProperties)
            {
                var initializer =
                    anonymousObject.Initializers.FirstOrDefault(i => GetName(i) == messageContractProperty.Name);

                if (initializer == null)
                {
                    var propertyToAdd = CreateProperty(messageContractProperty);
                    propertiesToAdd.Add(propertyToAdd);
                }
            }

            if (propertiesToAdd.Any())
            {
                var newAnonymousObject = anonymousObject
                    .AddInitializers(propertiesToAdd.ToArray())
                    .WithAdditionalAnnotations(Formatter.Annotation);
                newRoot = newRoot.ReplaceNode(anonymousObject, newAnonymousObject);
            }

            return newRoot;
        }

        private static AnonymousObjectMemberDeclaratorSyntax[] CreateProperties(ITypeSymbol messageContractType)
        {
            var messageContractProperties = GetMessageContractProperties(messageContractType);

            var propertiesToAdd = new List<AnonymousObjectMemberDeclaratorSyntax>();
            foreach (var messageContractProperty in messageContractProperties)
            {
                var propertyToAdd = CreateProperty(messageContractProperty);
                propertiesToAdd.Add(propertyToAdd);
            }

            return propertiesToAdd.ToArray();
        }

        private static AnonymousObjectMemberDeclaratorSyntax CreateProperty(IPropertySymbol messageContractProperty)
        {
            ExpressionSyntax expression;
            if (IsImmutableArray(messageContractProperty.Type, out var messageContractPropertyTypeArgument) ||
                IsReadOnlyList(messageContractProperty.Type, out messageContractPropertyTypeArgument))
            {
                expression = CreateImplicitArray(messageContractPropertyTypeArgument);
            }
            else if (messageContractProperty.Type.TypeKind == TypeKind.Interface)
            {
                expression = CreateAnonymousObject(messageContractProperty.Type);
            }
            else if (IsNullable(messageContractProperty.Type, out var nullableTypeArgument))
            {
                expression = CreateDefaultNullable(nullableTypeArgument);
            }
            else
            {
                expression = CreateDefault(messageContractProperty.Type);
            }

            return SyntaxFactory.AnonymousObjectMemberDeclarator(SyntaxFactory.NameEquals(messageContractProperty.Name), expression)
                .WithAdditionalAnnotations(Formatter.Annotation)
                .WithLeadingTrivia(SyntaxFactory.CarriageReturnLineFeed);
        }

        private static ImplicitArrayCreationExpressionSyntax CreateImplicitArray(ITypeSymbol type)
        {
            ExpressionSyntax node;
            if (type.TypeKind == TypeKind.Interface)
            {
                node = CreateAnonymousObject(type);
            }
            else
            {
                node = CreateDefault(type);
            }

            var nodes = new ExpressionSyntax[] { node };
            var initializer = SyntaxFactory.InitializerExpression(SyntaxKind.ArrayInitializerExpression)
                .WithExpressions(SyntaxFactory.SeparatedList(nodes));
            return SyntaxFactory.ImplicitArrayCreationExpression(initializer)
                .WithAdditionalAnnotations(Formatter.Annotation);
        }

        private static AnonymousObjectCreationExpressionSyntax CreateAnonymousObject(ITypeSymbol type)
        {
            var propertiesToAdd = CreateProperties(type);
            return SyntaxFactory.AnonymousObjectCreationExpression()
                .WithInitializers(SyntaxFactory.SeparatedList(propertiesToAdd))
                .WithAdditionalAnnotations(Formatter.Annotation);
        }

        private static DefaultExpressionSyntax CreateDefault(ITypeSymbol type)
        {
            return SyntaxFactory.DefaultExpression(SyntaxFactory.ParseTypeName(type.Name).WithAdditionalAnnotations(Simplifier.Annotation));
        }

        private static DefaultExpressionSyntax CreateDefaultNullable(ITypeSymbol type)
        {
            return SyntaxFactory.DefaultExpression(SyntaxFactory.NullableType(SyntaxFactory.ParseTypeName(type.Name).WithAdditionalAnnotations(Simplifier.Annotation)));
        }

        private static bool HasMessageContract(TypeSyntax typeArgument, SemanticModel semanticModel, out ITypeSymbol messageContractType)
        {
            if (typeArgument is IdentifierNameSyntax identifierNameSyntax)
            {
                var identifierType = semanticModel.GetTypeInfo(identifierNameSyntax).Type;
                if (identifierType.TypeKind == TypeKind.Interface)
                {
                    messageContractType = identifierType;
                    return true;
                }

                if (identifierType.TypeKind == TypeKind.TypeParameter &&
                    identifierType is ITypeParameterSymbol typeParameter &&
                    typeParameter.ConstraintTypes.Length == 1 &&
                    typeParameter.ConstraintTypes[0].TypeKind == TypeKind.Interface)
                {
                    messageContractType = typeParameter.ConstraintTypes[0];
                    return true;
                }
            }
            else if (typeArgument is GenericNameSyntax genericNameSyntax)
            {
                var genericType = semanticModel.GetTypeInfo(genericNameSyntax).Type;

                if (IsImmutableArray(genericType, out messageContractType) ||
                    IsReadOnlyList(genericType, out messageContractType) ||
                    IsList(genericType, out messageContractType))
                {
                    if (messageContractType.TypeKind == TypeKind.Interface)
                    {
                        return true;
                    }
                }
            }
            else if (typeArgument is ArrayTypeSyntax arrayTypeSyntax)
            {
                messageContractType = semanticModel.GetTypeInfo(arrayTypeSyntax.ElementType).Type;
                if (messageContractType.TypeKind == TypeKind.Interface)
                {
                    return true;
                }
            }
            else if (typeArgument is QualifiedNameSyntax qualifiedNameSyntax)
            {
                messageContractType = semanticModel.GetTypeInfo(qualifiedNameSyntax).Type;
                if (messageContractType.TypeKind == TypeKind.Interface)
                {
                    return true;
                }
            }

            messageContractType = null;
            return false;
        }

        private static bool IsActivator(ArgumentSyntax argumentSyntax, SemanticModel semanticModel, out TypeSyntax typeArgument)
        {
            if (argumentSyntax != null &&
                argumentSyntax.Parent is ArgumentListSyntax argumentListSyntax &&
                argumentListSyntax.Parent is InvocationExpressionSyntax invocationExpressionSyntax &&
                invocationExpressionSyntax.Expression is MemberAccessExpressionSyntax memberAccessExpressionSyntax &&
                memberAccessExpressionSyntax.Name is GenericNameSyntax genericNameSyntax &&
                genericNameSyntax.TypeArgumentList.Arguments.Count == 1 &&
                semanticModel.GetSymbolInfo(memberAccessExpressionSyntax).Symbol is IMethodSymbol method &&
                method.TypeArguments.Length == 1 &&
                method.Parameters[0].Type.SpecialType == SpecialType.System_Object &&
                method.ContainingType.ContainingAssembly.Name == "MassTransit" &&
                ((method.Name == "Publish" && method.ContainingType.Name == "IPublishEndpoint") || 
                (method.Name == "Send" && method.ContainingType.Name == "ISendEndpoint")))
            {
                typeArgument = genericNameSyntax.TypeArgumentList.Arguments[0];
                return true;
            }

            typeArgument = null;
            return false;
        }

        private static bool IsImmutableArray(ITypeSymbol type, out ITypeSymbol typeArgument)
        {
            if (type.TypeKind == TypeKind.Struct &&
                type.Name == "ImmutableArray" &&
                type.ContainingNamespace.ToString() == "System.Collections.Immutable" &&
                type is INamedTypeSymbol immutableArrayType &&
                immutableArrayType.IsGenericType &&
                immutableArrayType.TypeArguments.Length == 1)
            {
                typeArgument = immutableArrayType.TypeArguments[0];
                return true;
            }

            typeArgument = null;
            return false;
        }

        private static bool IsReadOnlyList(ITypeSymbol type, out ITypeSymbol typeArgument)
        {
            if (type.TypeKind == TypeKind.Interface &&
                type.Name == "IReadOnlyList" &&
                type.ContainingNamespace.ToString() == "System.Collections.Generic" &&
                type is INamedTypeSymbol readOnlyListType &&
                readOnlyListType.IsGenericType &&
                readOnlyListType.TypeArguments.Length == 1)
            {
                typeArgument = readOnlyListType.TypeArguments[0];
                return true;
            }

            typeArgument = null;
            return false;
        }

        private static bool IsList(ITypeSymbol type, out ITypeSymbol typeArgument)
        {
            if (type.TypeKind == TypeKind.Class &&
                type.Name == "List" &&
                type.ContainingNamespace.ToString() == "System.Collections.Generic" &&
                type is INamedTypeSymbol listType &&
                listType.IsGenericType &&
                listType.TypeArguments.Length == 1)
            {
                typeArgument = listType.TypeArguments[0];
                return true;
            }

            typeArgument = null;
            return false;
        }

        private static bool IsNullable(ITypeSymbol type, out ITypeSymbol typeArgument)
        {
            if (type.TypeKind == TypeKind.Struct &&
                type.Name == "Nullable" &&
                type.ContainingNamespace.Name == "System" &&
                type is INamedTypeSymbol nullableType &&
                nullableType.IsGenericType &&
                nullableType.TypeArguments.Length == 1)
            {
                typeArgument = nullableType.TypeArguments[0];
                return true;
            }

            typeArgument = null;
            return false;
        }
    }
}