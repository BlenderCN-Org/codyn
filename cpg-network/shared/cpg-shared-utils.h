#ifndef __CPG_SHARED_UTILS_H__
#define __CPG_SHARED_UTILS_H__

typedef unsigned CpgSharedPointer;

#define cpg_shared_pointer_type(value, Type) ((Type *)(value))
#define cpg_shared_pointer_base_type(base, offset, Type) cpg_shared_pointer_type((CpgSharedPointer)base + (CpgSharedPointer)offset, Type)

#define cpg_shared_array_base_type(base, first, index, Type) cpg_shared_pointer_base_type(base, first + sizeof(Type) * index, Type)

#define CPG_SHARED_OBJECT(base, offset) cpg_shared_pointer_base_type(base, offset, CpgSharedObject)
#define CPG_SHARED_LINK(base, offset) cpg_shared_pointer_base_type(base, offset, CpgSharedLink)
#define CPG_SHARED_PROPERTY(base, offset) cpg_shared_pointer_base_type(base, offset, CpgSharedProperty)
#define CPG_SHARED_EXPRESSION(base, offset) cpg_shared_pointer_base_type(base, offset, CpgSharedExpression)

#endif /* __CPG_SHARED_UTILS_H__ */

