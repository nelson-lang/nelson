//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "Types.hpp"
#include <complex>
//=============================================================================
namespace Nelson {
//=============================================================================
// Compile-time bridge that lets VariableTypedTableModel<T> speak to ArrayOf
// without hard-coding the double/single tokens. Each specialization maps the
// element type T to its Nelson class ids, type predicates, scalar extractors
// and scalar constructors.
template <typename T> struct VariableTypeTraits;
//=============================================================================
template <> struct VariableTypeTraits<double>
{
    static constexpr NelsonType RealClass = NLS_DOUBLE;
    static constexpr NelsonType ComplexClass = NLS_DCOMPLEX;
    static bool
    isType(const ArrayOf& a)
    {
        return a.isDoubleType(false);
    }
    static double
    asScalar(const ArrayOf& a)
    {
        return a.asDoubleScalar();
    }
    static std::complex<double>
    asComplexScalar(const ArrayOf& a)
    {
        return a.asDoubleComplexScalar();
    }
    static ArrayOf
    makeScalar(double v)
    {
        return ArrayOf::doubleScalar(v);
    }
    static ArrayOf
    makeComplexScalar(double re, double im)
    {
        return ArrayOf::dsingleComplexScalar(re, im);
    }
    static const char*
    typeName()
    {
        return "double";
    }
};
//=============================================================================
template <> struct VariableTypeTraits<single>
{
    static constexpr NelsonType RealClass = NLS_SINGLE;
    static constexpr NelsonType ComplexClass = NLS_SCOMPLEX;
    static bool
    isType(const ArrayOf& a)
    {
        return a.isSingleType(false);
    }
    static single
    asScalar(const ArrayOf& a)
    {
        return a.asSingleScalar();
    }
    static std::complex<single>
    asComplexScalar(const ArrayOf& a)
    {
        return a.asSingleComplexScalar();
    }
    static ArrayOf
    makeScalar(single v)
    {
        return ArrayOf::singleScalar(v);
    }
    static ArrayOf
    makeComplexScalar(single re, single im)
    {
        return ArrayOf::singleComplexScalar(re, im);
    }
    static const char*
    typeName()
    {
        return "single";
    }
};
//=============================================================================
} // namespace Nelson
//=============================================================================
