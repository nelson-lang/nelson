//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <fftw3.h>
//=============================================================================
namespace Nelson {
//=============================================================================
void
dyn_fftw_execute(const fftw_plan plan);
//=============================================================================
fftw_plan
dyn_fftw_plan_dft_1d(int n, fftw_complex* in, fftw_complex* out, int sign, unsigned flags);
//=============================================================================
void
dyn_fftw_destroy_plan(fftw_plan plan);
//=============================================================================
void
dyn_fftw_forget_wisdom(void);
//=============================================================================
char*
dyn_fftw_export_wisdom_to_string(void);
//=============================================================================
int
dyn_fftw_import_wisdom_from_string(const char* input_string);
//=============================================================================
void*
dyn_fftw_malloc(size_t n);
//=============================================================================
void
dyn_fftw_free(void* p);
//=============================================================================
void
dyn_fftwf_execute(const fftwf_plan plan);
//=============================================================================
void
dyn_fftwf_destroy_plan(fftwf_plan plan);
//=============================================================================
void
dyn_fftwf_free(void* p);
//=============================================================================
void*
dyn_fftwf_malloc(size_t n);
//=============================================================================
int
dyn_fftwf_import_wisdom_from_string(const char* input_string);
//=============================================================================
char*
dyn_fftwf_export_wisdom_to_string(void);
//=============================================================================
void
dyn_fftwf_forget_wisdom(void);
//=============================================================================
fftwf_plan
dyn_fftwf_plan_dft_1d(int n, fftwf_complex* in, fftwf_complex* out, int sign, unsigned flags);
//=============================================================================
}
//=============================================================================
