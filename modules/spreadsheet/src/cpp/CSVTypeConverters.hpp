//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <string>
#include <complex>
#include <Types.hpp>
//=============================================================================
namespace Nelson {
//=============================================================================
bool
ConvertToDouble(const std::string& pStr, double& pVal);
//=============================================================================
bool
ConvertToDoubleComplex(const std::string& str, std::complex<double>& pVal);
//=============================================================================
void
ConvertStringToDouble(const std::string& pStr, double& Val);
//=============================================================================
void
ConvertStringToSingle(const std::string& pStr, single& Val);
//=============================================================================
void
ConvertStringToDoubleComplex(const std::string& pStr, std::complex<double>& Val);
//=============================================================================
void
ConvertStringToSingleComplex(const std::string& pStr, std::complex <single> & Val);
//=============================================================================
void
ConvertStringToInt8(const std::string& pStr, int8& Val);
//=============================================================================
void
ConvertStringToInt16(const std::string& pStr, int16& Val);
//=============================================================================
void
ConvertStringToInt32(const std::string& pStr, int32& Val);
//=============================================================================
void
ConvertStringToInt64(const std::string& pStr, int64& Val);
//=============================================================================
void
ConvertStringToUInt8(const std::string& pStr, uint8& Val);
//=============================================================================
void
ConvertStringToUInt16(const std::string& pStr, uint16& Val);
//=============================================================================
void
ConvertStringToUInt32(const std::string& pStr, uint32& Val);
//=============================================================================
void
ConvertStringToUInt64(const std::string& pStr, uint64& Val);
//=============================================================================
}
//=============================================================================
