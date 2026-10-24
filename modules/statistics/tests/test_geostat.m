%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
%=============================================================================
asserts.isequal(nargin('geostat'), 1);
asserts.isequal(nargout('geostat'), 2);
%=============================================================================
[m, v] = geostat(0.25);
asserts.isapprox(m, 3, 1e-15);
asserts.isapprox(v, 12, 1e-15);
%=============================================================================
[m, v] = geostat([0.25 0.5 1 0]);
asserts.isapprox(m(1:3), [3 1 0], 1e-15);
asserts.isapprox(v(1:3), [12 2 0], 1e-15);
asserts.istrue(isinf(m(4)) && m(4) > 0);
asserts.istrue(isinf(v(4)) && v(4) > 0);
%=============================================================================
[m, v] = geostat(single([0.25 0.5]));
asserts.isequal(class(m), 'single');
asserts.isequal(class(v), 'single');
asserts.isapprox(double(m), [3 1], 1e-7);
asserts.isapprox(double(v), [12 2], 1e-6);
%=============================================================================
[m, v] = geostat([-0.1 1.1 NaN]);
asserts.istrue(all(isnan(m)));
asserts.istrue(all(isnan(v)));
%=============================================================================
[m, v] = geostat([]);
asserts.isequal(size(m), [0 0]);
asserts.isequal(size(v), [0 0]);
%=============================================================================
asserts.checkerror('geostat({0.5})', _('P must be a numeric array in the range [0,1].'));
%=============================================================================
[geoMean, geoVariance] = geostat(0.25);
asserts.isequal(geoMean, 3);
asserts.isequal(geoVariance, 12);
%=============================================================================
