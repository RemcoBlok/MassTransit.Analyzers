using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace MassTransit.Analyzers.MessageContractAnalyzer
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class MessageContractAnalyzerAnalyzer : DiagnosticAnalyzer
    {
        public const string StructurallyCompatibleRuleId = "MCA0001";
        public const string ValidMessageContractStructureRuleId = "MCA0002";
        public const string MissingPropertiesRuleId = "MCA0003";

        // You can change these strings in the Resources.resx file. If you do not want your analyzer to be localize-able, you can use regular strings for Title and MessageFormat.
        // See https://github.com/dotnet/roslyn/blob/master/docs/analyzers/Localizing%20Analyzers.md for more on localization

        private const string Category = "Usage";

        private static readonly DiagnosticDescriptor StructurallyCompatibleRule = new DiagnosticDescriptor(StructurallyCompatibleRuleId,
            "Anonymous type does not map to message contract",
            "Anonymous type does not map to message contract '{0}'. The following properties of the anonymous type are incompatible: {1}",
            Category, DiagnosticSeverity.Error, isEnabledByDefault: true,
            description: "Anonymous type should map to message contract");

        private static readonly DiagnosticDescriptor ValidMessageContractStructureRule = new DiagnosticDescriptor(ValidMessageContractStructureRuleId,
            "Message contract does not have a valid structure",
            "Message contract '{0}' does not have a valid structure",
            Category, DiagnosticSeverity.Error, isEnabledByDefault: true,
            description: "Message contract should have a valid structure. Properties should be primitive, string or IReadOnlyList or ImmatableArray of a primitive, string or message contract");

        private static readonly DiagnosticDescriptor MissingPropertiesRule = new DiagnosticDescriptor(MissingPropertiesRuleId,
            "Anonymous type is missing properties that are in the message contract",
            "Anonymous type is missing properties that are in the message contract '{0}'. The following properties are missing: {1}",
            Category, DiagnosticSeverity.Info, isEnabledByDefault: true,
            description: "Anonymous type misses properties that are in the message contract");

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(StructurallyCompatibleRule, ValidMessageContractStructureRule, MissingPropertiesRule);

        public override void Initialize(AnalysisContext context)
        {
            if (context == null)
            {
                throw new ArgumentNullException(nameof(context));
            }

            context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.None);
            context.EnableConcurrentExecution();
            // TODO: Consider registering other actions that act on syntax instead of or in addition to symbols
            // See https://github.com/dotnet/roslyn/blob/master/docs/analyzers/Analyzer%20Actions%20Semantics.md for more information
            context.RegisterSyntaxNodeAction(AnalyzeNode, SyntaxKind.AnonymousObjectCreationExpression);
        }

        private static void AnalyzeNode(SyntaxNodeAnalysisContext context)
        {
            var anonymousObjectCreationExpressionSyntax = (AnonymousObjectCreationExpressionSyntax)context.Node;
            var argumentSyntax = anonymousObjectCreationExpressionSyntax.Parent as ArgumentSyntax;
            if (argumentSyntax == null)
            {
                if (anonymousObjectCreationExpressionSyntax.Parent is InitializerExpressionSyntax initializerExpressionSyntax &&
                    initializerExpressionSyntax.Parent is ImplicitArrayCreationExpressionSyntax implicitArrayCreationExpressionSyntax)
                {
                    argumentSyntax = implicitArrayCreationExpressionSyntax.Parent as ArgumentSyntax;
                }
            }

            if (IsActivator(argumentSyntax, context.SemanticModel, out var typeArgument))
            {
                var anonymousType = context.SemanticModel.GetTypeInfo(anonymousObjectCreationExpressionSyntax).Type;

                if (HasMessageContract(typeArgument, context.SemanticModel, out var messageContractType))
                {
                    var incompatibleProperties = new List<string>();
                    if (!TypesAreStructurallyCompatible(anonymousType, messageContractType, string.Empty, incompatibleProperties))
                    {
                        var diagnostic = Diagnostic.Create(StructurallyCompatibleRule, anonymousType.Locations[0],
                            messageContractType.Name, string.Join(", ", incompatibleProperties));
                        context.ReportDiagnostic(diagnostic);
                    }

                    var missingProperties = new List<string>();
                    if (HasMissingProperties(anonymousType, messageContractType, string.Empty, missingProperties))
                    {
                        var diagnostic = Diagnostic.Create(MissingPropertiesRule, anonymousType.Locations[0],
                            messageContractType.Name, string.Join(", ", missingProperties));
                        context.ReportDiagnostic(diagnostic);
                    }
                }
                else
                {
                    var diagnostic = Diagnostic.Create(ValidMessageContractStructureRule, anonymousType.Locations[0], typeArgument.GetText());
                    context.ReportDiagnostic(diagnostic);
                }
            }
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

        private static bool IsArray(ITypeSymbol type, out ITypeSymbol elementType)
        {
            if (type.TypeKind == TypeKind.Array &&
                type is IArrayTypeSymbol arrayTypeSymbol)
            {
                elementType = arrayTypeSymbol.ElementType;
                return true;
            }

            elementType = null;
            return false;
        }

        private static bool TypesAreStructurallyCompatible(ITypeSymbol messageType, ITypeSymbol messageContractType, string path, ICollection<string> incompatibleProperties)
        {
            if (SymbolEqualityComparer.Default.Equals(messageType, messageContractType))
            {
                return true;
            }

            var messageContractProperties = GetMessageContractProperties(messageContractType);
            var messageProperties = messageType.GetMembers().OfType<IPropertySymbol>().ToList();

            foreach (var messageProperty in messageProperties)
            {
                var messageContractProperty =
                    messageContractProperties.FirstOrDefault(m => m.Name == messageProperty.Name);

                if (messageContractProperty == null)
                {
                    incompatibleProperties.Add(Append(path, messageProperty.Name));
                    return false;
                }

                if (!SymbolEqualityComparer.Default.Equals(messageProperty.Type, messageContractProperty.Type))
                {
                    if (messageProperty.Type.IsAnonymousType)
                    {
                        if (messageContractProperty.Type.TypeKind == TypeKind.Interface)
                        {
                            if (!TypesAreStructurallyCompatible(messageProperty.Type, messageContractProperty.Type,
                                Append(path, messageProperty.Name), incompatibleProperties))
                            {
                                return false;
                            }
                        }
                        else
                        {
                            incompatibleProperties.Add(Append(path, messageProperty.Name));
                            return false;
                        }
                    }
                    else if (IsImmutableArray(messageProperty.Type, out var messagePropertyTypeArgument) ||
                             IsReadOnlyList(messageProperty.Type, out messagePropertyTypeArgument) ||
                             IsList(messageProperty.Type, out messagePropertyTypeArgument) ||
                             IsArray(messageProperty.Type, out messagePropertyTypeArgument))
                    {
                        if (IsImmutableArray(messageContractProperty.Type, out var messageContractPropertyTypeArgument) ||
                            IsReadOnlyList(messageContractProperty.Type, out messageContractPropertyTypeArgument))
                        {
                            if (!SymbolEqualityComparer.Default.Equals(messagePropertyTypeArgument, messageContractPropertyTypeArgument))
                            {
                                if (messageContractPropertyTypeArgument.TypeKind == TypeKind.Interface)
                                {
                                    if (!TypesAreStructurallyCompatible(messagePropertyTypeArgument, messageContractPropertyTypeArgument,
                                        Append(path, messageProperty.Name), incompatibleProperties))
                                    {
                                        return false;
                                    }
                                }
                                else
                                {
                                    incompatibleProperties.Add(Append(path, messageProperty.Name));
                                    return false;
                                }
                            }
                        }
                        else
                        {
                            incompatibleProperties.Add(Append(path, messageProperty.Name));
                            return false;
                        }
                    }
                    else
                    {
                        incompatibleProperties.Add(Append(path, messageProperty.Name));
                        return false;
                    }
                }
            }

            return true;
        }

        private static bool HasMissingProperties(ITypeSymbol messageType, ITypeSymbol messageContractType, string path, ICollection<string> missingProperties)
        {
            var messageContractProperties = GetMessageContractProperties(messageContractType);
            var messageProperties = messageType.GetMembers().OfType<IPropertySymbol>().ToList();
            var result = false;

            foreach (var messageContractProperty in messageContractProperties)
            {
                var messageProperty =
                    messageProperties.FirstOrDefault(m => m.Name == messageContractProperty.Name);

                if (messageProperty == null)
                {
                    missingProperties.Add(Append(path, messageContractProperty.Name));
                    result = true;
                }
                else if (IsImmutableArray(messageContractProperty.Type, out var messageContractPropertyTypeArgument) ||
                    IsReadOnlyList(messageContractProperty.Type, out messageContractPropertyTypeArgument))
                {
                    if (messageContractPropertyTypeArgument.TypeKind == TypeKind.Interface)
                    {
                        if (IsImmutableArray(messageProperty.Type, out var messagePropertyTypeArgument) ||
                            IsReadOnlyList(messageProperty.Type, out messagePropertyTypeArgument) ||
                            IsArray(messageProperty.Type, out messagePropertyTypeArgument))
                        {
                            var hasMissingProperties = HasMissingProperties(messagePropertyTypeArgument, messageContractPropertyTypeArgument,
                                Append(path, messageContractProperty.Name), missingProperties);
                            if (hasMissingProperties)
                            {
                                result = true;
                            }
                        }
                    }
                }
                else if (messageContractProperty.Type.TypeKind == TypeKind.Interface)
                {
                    if (messageProperty.Type.IsAnonymousType)
                    {
                        var hasMissingProperties = HasMissingProperties(messageProperty.Type, messageContractProperty.Type,
                            Append(path, messageContractProperty.Name), missingProperties);
                        if (hasMissingProperties)
                        {
                            result = true;
                        }
                    }
                }
            }

            return result;
        }

        private static List<IPropertySymbol> GetMessageContractProperties(ITypeSymbol messageContractType)
        {
            var messageContractTypes = new List<ITypeSymbol> { messageContractType };
            messageContractTypes.AddRange(messageContractType.AllInterfaces);

            return messageContractTypes.SelectMany(i => i.GetMembers().OfType<IPropertySymbol>()).ToList();
        }

        private static string Append(string path, string propertyName)
        {
            if (string.IsNullOrEmpty(path))
            {
                return propertyName;
            }

            if (path.EndsWith(".", StringComparison.Ordinal))
            {
                return $"{path}{propertyName}";
            }

            return $"{path}.{propertyName}";
        }
    }
}