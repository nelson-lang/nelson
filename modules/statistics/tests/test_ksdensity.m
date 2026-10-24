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
asserts.isequal(nargin('ksdensity'), -3);
asserts.isequal(nargout('ksdensity'), 3);
%=============================================================================
[f, xi, bw] = ksdensity([0 1 2], [0 1 2], 'Bandwidth', 1);
asserts.isapprox(f, [0.231634657144588; 0.294294576479907; 0.231634657144588], 1e-12);
asserts.isequal(xi, [0; 1; 2]);
asserts.isequal(bw, 1);
%=============================================================================
[f, xi, bw] = ksdensity([0 1 2]);
asserts.isequal(size(f), [100 1]);
asserts.isapprox(xi(1), -2.552718166397534, 1e-12);
asserts.isapprox(xi(end), 4.552718166397534, 1e-12);
asserts.isapprox(bw, 0.850906055465845, 1e-12);
%=============================================================================
F = ksdensity([0 1 2], [0 1 2], 'Bandwidth', 1, 'Function', 'cdf');
asserts.isapprox(F, [0.227135128626545; 0.5; 0.772864871373454], 1e-12);
S = ksdensity([0 1 2], [0 1 2], 'Bandwidth', 1, 'Function', 'survivor');
asserts.isapprox(S, [0.772864871373455; 0.5; 0.227135128626546], 1e-12);
H = ksdensity([0 1 2], [0 1 2], 'Bandwidth', 1, 'Function', 'cumhazard');
asserts.isapprox(H, [0.257651056318238; 0.693147180559945; 1.482210158316613], 1e-12);
%=============================================================================
f = ksdensity([0 1], [0 1], 'Bandwidth', 1, 'Weights', [1 3]);
asserts.isapprox(f, [0.281213613489716; 0.359699391430860], 1e-12);
%=============================================================================
f = ksdensity([1 2 3], [-1 1 4], 'Bandwidth', 1, 'Support', 'positive');
asserts.isapprox(f, [0; 0.231634657144588; 0.100131179814756], 1e-12);
%=============================================================================
q = ksdensity([0 1 2], [0.25 0.5 0.75], 'Bandwidth', 1, 'Function', 'icdf');
asserts.isapprox(q, [0.096110498950237; 1; 1.903889501049763], 1e-12);
%=============================================================================
asserts.checkerror('ksdensity()', _('Not enough input arguments.'));
asserts.checkerror('ksdensity({1})', sprintf(_('%s must be a real numeric vector.'), 'X'));
asserts.checkerror('ksdensity([1 2], [1 2], ''Weights'', [1 2 3])', sprintf(_('%s must match X.'), 'Weights'));
asserts.checkerror('ksdensity([1 2], [1 2], ''Bandwidth'', 0)', sprintf(_('%s must be a positive scalar.'), 'Bandwidth'));
asserts.checkerror('ksdensity([1 2], [1 2], ''Function'', ''bad'')', sprintf(_('Unsupported %s.'), 'function type'));
asserts.checkerror('ksdensity([1 2], [1 2], ''Kernel'', ''box'')', _('Only normal kernel is supported.'));
asserts.checkerror('ksdensity([1 2], [1 2], ''Support'', [2 1])', _('Support must be a two-element increasing vector.'));
%=============================================================================
