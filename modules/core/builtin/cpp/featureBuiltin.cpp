//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "featureBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "NelsonConfiguration.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
typedef ArrayOf (*MethodGetPtr)();
typedef ArrayOf (*MethodSetPtr)(const ArrayOf& value);
//=============================================================================
static ArrayOf
currentAxesOnClick();
static ArrayOf
currentFigureOnClick();
static ArrayOf
ompEnabled();
static ArrayOf
ompThreshold();
//=============================================================================
static ArrayOf
setCurrentAxesOnClick(const ArrayOf& value);
static ArrayOf
setCurrentFigureOnClick(const ArrayOf& value);
static ArrayOf
setOmpEnabled(const ArrayOf& value);
static ArrayOf
setOmpThreshold(const ArrayOf& value);
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::featureBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 2);

    std::wstring fieldname = argIn[0].asWideString();

    if (argIn.size() == 1) {
        std::map<std::wstring, MethodGetPtr> methodMap;
        methodMap[L"currentAxesOnClick"] = &currentAxesOnClick;
        methodMap[L"currentFigureOnClick"] = &currentFigureOnClick;
        methodMap[L"ompEnabled"] = &ompEnabled;
        methodMap[L"ompThreshold"] = &ompThreshold;

        if (methodMap.count(fieldname) == 1) {
            retval << (methodMap[fieldname])();
        } else {
            Error(L"Nelson:feature:invalidValue", L"Wrong value for #%d argument.", 1);
        }
    } else {
        ArrayOf fieldvalue = argIn[1];
        std::map<std::wstring, MethodSetPtr> methodMap;
        methodMap[L"currentAxesOnClick"] = &setCurrentAxesOnClick;
        methodMap[L"currentFigureOnClick"] = &setCurrentFigureOnClick;
        methodMap[L"ompEnabled"] = &setOmpEnabled;
        methodMap[L"ompThreshold"] = &setOmpThreshold;

        if (methodMap.count(fieldname) == 1) {
            retval << (methodMap[fieldname])(fieldvalue);
        } else {
            Error(L"Nelson:feature:invalidValue", L"Wrong value for #%d argument.", 1);
        }
    }
    return retval;
}
//=============================================================================
ArrayOf
currentAxesOnClick()
{
    return ArrayOf::logicalScalar(NelsonConfiguration::getInstance()->isCurrentAxesOnClick());
}
//=============================================================================
ArrayOf
currentFigureOnClick()
{
    return ArrayOf::logicalScalar(NelsonConfiguration::getInstance()->isCurrentFigureOnClick());
}
//=============================================================================
ArrayOf
ompEnabled()
{
    return ArrayOf::logicalScalar(
        NelsonConfiguration::getInstance()->isOpenMPParallelizationEnabled());
}
//=============================================================================
ArrayOf
ompThreshold()
{
    return ArrayOf::doubleScalar(
        (double)NelsonConfiguration::getInstance()->getOpenMPParallelizationThreshold());
}
//=============================================================================
ArrayOf
setCurrentAxesOnClick(const ArrayOf& value)
{
    NelsonConfiguration::getInstance()->setCurrentAxesOnClick(value.asLogicalScalar());
    return ArrayOf();
}
//=============================================================================
ArrayOf
setCurrentFigureOnClick(const ArrayOf& value)
{
    NelsonConfiguration::getInstance()->setCurrentFigureOnClick(value.asLogicalScalar());
    return ArrayOf();
}
//=============================================================================
ArrayOf
setOmpEnabled(const ArrayOf& value)
{
    NelsonConfiguration::getInstance()->enableOpenMPParallelization(value.asLogicalScalar());
    return ArrayOf();
}
//=============================================================================
ArrayOf
setOmpThreshold(const ArrayOf& value)
{
    NelsonConfiguration::getInstance()->setOpenMPParallelizationThreshold(
        (uint64_t)value.asDoubleScalar());
    return ArrayOf();
}
//=============================================================================
