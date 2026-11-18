%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% sinc has a removable singularity at 0 and decays to NaN at infinity
asserts.isequal(sinc(0), 1);
asserts.istrue(isnan(sinc(Inf)));
asserts.isequal(size(sinc([])), [0 0]);
%=============================================================================
% decibel conversions at the zero / unity boundaries
asserts.isequal(db2mag(0), 1);
asserts.isequal(db2pow(0), 1);
asserts.istrue(isinf(mag2db(0)) && mag2db(0) < 0);
asserts.istrue(isinf(pow2db(0)) && pow2db(0) < 0);
%=============================================================================
% a NaN sample contaminates rms unless it is explicitly omitted
asserts.istrue(isnan(rms([1 NaN 3])));
asserts.isapprox(rms([1 NaN 3], 'omitnan'), sqrt(5), 1e-12);
%=============================================================================
% peak-to-peak of a single sample is zero, and a NaN is ignored by the span
asserts.isequal(peak2peak(single(5)), single(0));
asserts.isequal(peak2peak([1 NaN 3]), 2);
%=============================================================================
