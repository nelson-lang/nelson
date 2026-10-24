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
asserts.isequal(nargin('evcdf'), 4);
asserts.isequal(nargout('evcdf'), 1);
%=============================================================================
x = [-Inf -2 -1 0 1 2 Inf NaN];
R = evcdf(x);
E = [0 0.1265769815068834 0.3077993724446536 0.6321205588285577 ...
     0.9340119641546875 0.9993820210106689 1 NaN];
asserts.isapprox(R(1:7), E(1:7), 1e-15);
asserts.istrue(isnan(R(8)));
%=============================================================================
R = evcdf(x, 0, 1, 'upper');
E = [1 0.8734230184931167 0.6922006275553464 0.3678794411714423 ...
     0.06598803584531254 0.0006179789893310934 0 NaN];
asserts.isapprox(R(1:7), E(1:7), 1e-15);
asserts.istrue(isnan(R(8)));
%=============================================================================
R = evcdf([-2 -1 0 1 2], [0 0 1 1 2], [1 2 1 3 2]);
E = [0.1265769815068834 0.4547607881073948 0.3077993724446536 ...
     0.6321205588285577 0.6321205588285577];
asserts.isapprox(R, E, 1e-15);
%=============================================================================
R = evcdf(single([-2 -1 0 1 2]), single(0), single(1), 'u');
asserts.isequal(class(R), 'single');
asserts.isapprox(double(R), [0.8734230184931167 0.6922006275553464 ...
                            0.3678794411714423 0.06598803584531254 ...
                            0.0006179789893310934], 1e-7);
%=============================================================================
R = evcdf([1 NaN 1 1], [0 0 NaN 0], [1 1 1 -1]);
asserts.isapprox(R(1), 0.9340119641546875, 1e-15);
asserts.istrue(all(isnan(R(2:4))));
%=============================================================================
R = evcdf([]);
asserts.isequal(size(R), [0 0]);
R = evcdf([], [], []);
asserts.isequal(size(R), [0 0]);
%=============================================================================
asserts.checkerror('evcdf(ones(2), ones(3), 1)', _('Size mismatch on input arguments'));
asserts.checkerror('evcdf(1, 0, 1, ''bad'')', sprintf(_('Invalid %s distribution option.'), 'extreme value'));
asserts.checkerror('evcdf(1, 0, 1, ''upper'', ''upper'')', _('Too many input arguments.'));
asserts.checkerror('evcdf({1}, 0, 1)', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('evcdf(1, {0}, 1)', sprintf(_('%s must be real numeric.'), 'MU'));
asserts.checkerror('evcdf(1, 0, {1})', sprintf(_('%s must be real numeric.'), 'SIGMA'));
%=============================================================================
