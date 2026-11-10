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
// functions without a @gpuArray method whose generic code runs on gpuArray inputs
#define GPU_GENERIC_FUNCTIONS(X)                                                                   \
    X(allunique)                                                                                   \
    X(anymissing)                                                                                  \
    X(anynan)                                                                                      \
    X(betaincinv)                                                                                  \
    X(cart2pol)                                                                                    \
    X(cart2sph)                                                                                    \
    X(colperm)                                                                                     \
    X(cond)                                                                                        \
    X(convn)                                                                                       \
    X(cov)                                                                                         \
    X(createArray)                                                                                 \
    X(curl)                                                                                        \
    X(deconv)                                                                                      \
    X(del2)                                                                                        \
    X(detrend)                                                                                     \
    X(fillmissing)                                                                                 \
    X(filter2)                                                                                     \
    X(freqspace)                                                                                   \
    X(gammaincinv)                                                                                 \
    X(getrangefromclass)                                                                           \
    X(gradient)                                                                                    \
    X(gsvd)                                                                                        \
    X(head)                                                                                        \
    X(height)                                                                                      \
    X(im2gray)                                                                                     \
    X(ind2sub)                                                                                     \
    X(inpolygon)                                                                                   \
    X(isoutlier)                                                                                   \
    X(issorted)                                                                                    \
    X(isUnderlyingType)                                                                            \
    X(isuniform)                                                                                   \
    X(legendre)                                                                                    \
    X(logspace)                                                                                    \
    X(lscov)                                                                                       \
    X(lsqminnorm)                                                                                  \
    X(makima)                                                                                      \
    X(mape)                                                                                        \
    X(mode)                                                                                        \
    X(movmad)                                                                                      \
    X(movmedian)                                                                                   \
    X(nonzeros)                                                                                    \
    X(normalize)                                                                                   \
    X(normest)                                                                                     \
    X(null)                                                                                        \
    X(numunique)                                                                                   \
    X(orth)                                                                                        \
    X(paddata)                                                                                     \
    X(pagectranspose)                                                                              \
    X(pagelsqminnorm)                                                                              \
    X(pagemldivide)                                                                                \
    X(pagemrdivide)                                                                                \
    X(pagenorm)                                                                                    \
    X(pagepinv)                                                                                    \
    X(pagesvd)                                                                                     \
    X(pchip)                                                                                       \
    X(perms)                                                                                       \
    X(pinv)                                                                                        \
    X(pol2cart)                                                                                    \
    X(poly)                                                                                        \
    X(polyarea)                                                                                    \
    X(polyder)                                                                                     \
    X(polyfit)                                                                                     \
    X(polyint)                                                                                     \
    X(polyvalm)                                                                                    \
    X(ppval)                                                                                       \
    X(psi)                                                                                         \
    X(rank)                                                                                        \
    X(rectint)                                                                                     \
    X(rescale)                                                                                     \
    X(resize)                                                                                      \
    X(rmmissing)                                                                                   \
    X(rmoutliers)                                                                                  \
    X(rmse)                                                                                        \
    X(sortrows)                                                                                    \
    X(sph2cart)                                                                                    \
    X(subspace)                                                                                    \
    X(tail)                                                                                        \
    X(tensorprod)                                                                                  \
    X(tfqmr)                                                                                       \
    X(trimdata)                                                                                    \
    X(uniquetol)                                                                                   \
    X(unwrap)                                                                                      \
    X(width)                                                                                       \
    X(xcorr)                                                                                       \
    X(xcov)
//=============================================================================
