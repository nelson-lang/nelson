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
asserts.isequal(nargin('statget'), 3);
asserts.isequal(nargout('statget'), 1);
%=============================================================================
opts = statset('kmeans');
asserts.isequal(statget(opts, 'Display'), 'off');
asserts.isequal(statget(opts, 'MaxI'), 100);
asserts.isequal(statget(opts, "UseParallel"), false);
%=============================================================================
asserts.isequal(statget(opts, 'TolX'), []);
asserts.isequal(statget(opts, 'TolX', 1e-8), 1e-8);
asserts.isequal(statget(opts, 'Display', 'iter'), 'off');
%=============================================================================
asserts.isequal(statget(opts, 'Tol'), []);
asserts.isequal(statget(opts, 'NotAField'), []);
asserts.isequal(statget(opts, 'NotAField', 123), []);
%=============================================================================
custom = struct('Alpha', [], 'Alphabet', 2, 'Beta', 3);
asserts.isequal(statget(custom, 'Alpha'), []);
asserts.isequal(statget(custom, 'Alpha', 5), 5);
asserts.isequal(statget(custom, 'Alphab'), 2);
asserts.isequal(statget(custom, 'Al'), []);
%=============================================================================
asserts.checkerror('statget()', _('Not enough input arguments.'));
asserts.checkerror('statget(1, ''Display'')', sprintf(_('%s must be a scalar structure.'), 'OPTIONS'));
asserts.checkerror('statget(opts, 1)', sprintf(_('%s must be a character vector or string scalar.'), 'FIELD'));
%=============================================================================
