using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace MassTransit.Analyzers
{
    public static class CommonExpressions
    {
        public static bool IsActivator(this ArgumentSyntax argumentSyntax, SemanticModel semanticModel, out ITypeSymbol messageContractType)
        {
            if (argumentSyntax != null
                && argumentSyntax.Parent is ArgumentListSyntax argumentListSyntax
                && argumentListSyntax.Parent is InvocationExpressionSyntax invocationExpressionSyntax
                && invocationExpressionSyntax.Expression is MemberAccessExpressionSyntax memberAccessExpressionSyntax
                && memberAccessExpressionSyntax.Name is GenericNameSyntax genericNameSyntax
                && semanticModel.GetSymbolInfo(memberAccessExpressionSyntax).Symbol is IMethodSymbol method
                && method.Parameters[0].Type.SpecialType == SpecialType.System_Object)
            {
                if (genericNameSyntax.TypeArgumentList.Arguments.Count == 1
                    && method.TypeArguments.Length == 1
                    && IsGenericInitializerMethod(method))
                {
                    return HasMessageContract(genericNameSyntax.TypeArgumentList.Arguments[0], semanticModel, out messageContractType);
                }

                if (method.ContainingType.IsGenericType
                    && method.ContainingType.TypeArguments.Length == 1
                    && IsInitializerMethod(method))
                {
                    messageContractType = method.ContainingType.TypeArguments[0];
                    return true;
                }
            }

            messageContractType = null;
            return false;
        }

        static bool IsGenericInitializerMethod(IMethodSymbol method)
        {
            return method != null
                   && method.ContainingType.ContainingAssembly.Name == "MassTransit"
                   && (method.Name == "Publish" && method.ContainingType.Name == "IPublishEndpoint"
                       || method.Name == "Send" && method.ContainingType.Name == "ISendEndpoint"
                       || method.Name == "Create" && method.ContainingType.Name == "IRequestClient"
                       || method.Name == "RespondAsync" && method.ContainingType.Name == "ConsumeContext");
        }

        static bool IsInitializerMethod(IMethodSymbol method)
        {
            return method != null
                   && method.ContainingType.ContainingAssembly.Name == "MassTransit"
                   && method.Name == "Create" && method.ContainingType.Name == "IRequestClient";
        }

        static bool HasMessageContract(TypeSyntax typeArgument, SemanticModel semanticModel, out ITypeSymbol messageContractType)
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


        static bool IsImmutableArray(ITypeSymbol type, out ITypeSymbol typeArgument)
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

        static bool IsReadOnlyList(ITypeSymbol type, out ITypeSymbol typeArgument)
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