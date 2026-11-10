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
// Amos complex Bessel and Airy routines (ACM TOMS 644), one based arrays of n values.
namespace Nelson::Amos {
//=============================================================================
void
zbesj(double zr, double zi, double fnu, int kode, int n, double* cyr, double* cyi, int& nz,
    int& ierr);
//=============================================================================
void
zbesy(double zr, double zi, double fnu, int kode, int n, double* cyr, double* cyi, int& nz,
    double* cwrkr, double* cwrki, int& ierr);
//=============================================================================
void
zbesi(double zr, double zi, double fnu, int kode, int n, double* cyr, double* cyi, int& nz,
    int& ierr);
//=============================================================================
void
zbesk(double zr, double zi, double fnu, int kode, int n, double* cyr, double* cyi, int& nz,
    int& ierr);
//=============================================================================
void
zbesh(double zr, double zi, double fnu, int kode, int m, int n, double* cyr, double* cyi, int& nz,
    int& ierr);
//=============================================================================
void
zairy(double zr, double zi, int id, int kode, double& air, double& aii, int& nz, int& ierr);
//=============================================================================
void
zbiry(double zr, double zi, int id, int kode, double& bir, double& bii, int& ierr);
//=============================================================================
} // namespace Nelson::Amos
//=============================================================================
