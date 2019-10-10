/*
 * Elaborated types are AstType::Elaboration(obj_ty, trait_ty).
 * They parse like <obj_ty as trait_ty>::...
 *
 * Static calls with elaborated types as their child are unpackaged into an unelaborated form.
 *
 * Associated types with elaborated types as their child are unpackaged into an unelaborated form.
 * E.g. AssociatedType { obj_type = ElaboratedType(obj_ty1, trait_ty1), trait_ty = None } =>
          AssociatedType { obj_type = obj_ty1, trait_ty = trait_ty1 }.
     See how the ElaboratedType is destructed and the associated obj and trait tys are plugged in?
 *
 * Otherwise, elaborated types are errors "unexpected elaborated type in non-trait context" or smth.
 *
 * Cool way to deal with <_ as Trait>:: types without having to majorly modify the parser,
 * since elaborated types can then just be parsed like regular types and then dealt with an adapter
 * later...
 */