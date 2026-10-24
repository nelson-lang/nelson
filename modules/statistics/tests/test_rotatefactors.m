%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--REUSE PROCESS-->
%=============================================================================
asserts.isequal(nargin('rotatefactors'), -2);
asserts.isequal(nargout('rotatefactors'), 2);
%=============================================================================
X = [0.8 0.2; 0.7 -0.1; 0.1 0.9; 0.2 0.8];
[B, T] = rotatefactors(X);
asserts.isapprox(B, [7.824664113322017e-01 2.602812231739080e-01; ...
                    7.055790164965421e-01 -4.645698526349132e-02; ...
                    3.124178047740718e-02 9.049994205261137e-01; ...
                    1.385595848852350e-01 8.128968209043700e-01], 1e-9);
asserts.isapprox(T, [9.971020201478573e-01 7.607602393042062e-02; ...
                   -7.607602393042062e-02 9.971020201478573e-01], 1e-9);
asserts.isapprox(B, X * T, 1e-9);
%=============================================================================
[B2, T2] = rotatefactors(X, 'Method', 'promax');
asserts.isapprox(B2, [7.626143483071012e-01 1.344558431134069e-01; ...
                     7.071027038275292e-01 -1.564451322523653e-01; ...
                    -3.135777164875295e-02 8.889242371328911e-01; ...
                     8.206681691636908e-02 7.811430379872506e-01], 1e-9);
asserts.isapprox(T2, [9.894635254373452e-01 -8.110655517787442e-02; ...
                    -1.447823602138750e-01 9.967054362785318e-01], 1e-9);
asserts.isapprox(B2, X * T2, 1e-9);
%=============================================================================
target = [1 0; 1 0; 0 1; 0 1];
[B3, T3] = rotatefactors(X, 'Method', 'procrustes', 'Target', target, 'Type', 'orthogonal');
asserts.isapprox(B3, [7.859664055552749e-01 2.495131446207222e-01; ...
                     7.048746335535400e-01 -5.614045753966249e-02; ...
                     4.366480030862638e-02 9.044851492501177e-01; ...
                     1.497078867724333e-01 8.109177200173470e-01], 1e-9);
asserts.isapprox(T3, [9.980525784828885e-01 6.237828615518054e-02; ...
                    -6.237828615518054e-02 9.980525784828885e-01], 1e-9);
asserts.isapprox(B3, X * T3, 1e-9);
%=============================================================================
[Bs, Ts] = rotatefactors(single(X), 'Normalize', 'off', 'Method', 'quartimax');
asserts.isequal(class(Bs), 'single');
asserts.isequal(class(Ts), 'single');
asserts.isequal(size(Bs), [4 2]);
asserts.isequal(size(Ts), [2 2]);
%=============================================================================
asserts.checkerror('rotatefactors()', _('Not enough input arguments.'));
asserts.checkerror('rotatefactors({1})', sprintf(_('%s must be a real numeric matrix.'), 'X'));
asserts.checkerror('rotatefactors([1 NaN])', sprintf(_('%s must contain finite values.'), 'X'));
asserts.checkerror('rotatefactors(X, ''Method'', ''bad'')', sprintf(_('%s has an unsupported value.'), 'Method'));
asserts.checkerror('rotatefactors(X, ''Normalize'', ''bad'')', sprintf(_('%s has an unsupported value.'), 'Normalize'));
asserts.checkerror('rotatefactors(X, ''RelTol'', 0)', sprintf(_('%s must be a positive finite scalar.'), 'RelTol'));
asserts.checkerror('rotatefactors(X, ''MaxIt'', 0)', sprintf(_('%s must be a positive integer scalar.'), 'MaxIt'));
asserts.checkerror('rotatefactors(X, ''Coeff'', -1)', _('Coeff must be a nonnegative finite scalar.'));
asserts.checkerror('rotatefactors(X, ''Power'', 0)', sprintf(_('%s must be a positive finite scalar.'), 'Power'));
asserts.checkerror('rotatefactors(X, ''Method'', ''procrustes'')', _('Target is required for this rotation method.'));
asserts.checkerror('rotatefactors(X, ''Method'', ''pattern'', ''Target'', ones(3, 2))', _('Target must have the same size as X.'));
asserts.checkerror('rotatefactors(X, ''Unknown'', 1)', _('Unknown option.'));
%=============================================================================
