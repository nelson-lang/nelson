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
asserts.isequal(nargin('pcacov'), 1);
asserts.isequal(nargout('pcacov'), 3);
%=============================================================================
V = [4 2; 2 3];
[coeff, latent, explained] = pcacov(V);
asserts.isapprox(coeff, [0.788205438016109 -0.615412209402636; ...
  0.615412209402636 0.788205438016109], 1e-12);
asserts.isapprox(coeff' * coeff, eye(2), 1e-12);
asserts.isapprox(latent, [5.561552812808831; 1.438447187191170], 1e-12);
asserts.isapprox(explained, [79.450754468697568; 20.549245531302425], 1e-12);
asserts.isapprox(sum(explained), 100, 1e-12);
%=============================================================================
[coeff, latent, explained] = pcacov(single(V));
asserts.isequal(class(coeff), 'single');
asserts.isequal(class(latent), 'single');
asserts.isequal(class(explained), 'single');
asserts.isapprox(double(coeff), [0.788205438016109 -0.615412209402636; ...
  0.615412209402636 0.788205438016109], 1e-6);
%=============================================================================
[coeff, latent, explained] = pcacov(zeros(2));
asserts.isequal(coeff, eye(2));
asserts.isequal(latent, [0; 0]);
asserts.isequal(explained, [0; 0]);
%=============================================================================
[coeff, latent, explained] = pcacov([]);
asserts.isequal(size(coeff), [0 0]);
asserts.isequal(size(latent), [0 1]);
asserts.isequal(size(explained), [0 1]);
%=============================================================================
asserts.checkerror('pcacov()', _('Not enough input arguments.'));
asserts.checkerror('pcacov({1})', sprintf(_('%s must be a real numeric matrix.'), 'V'));
asserts.checkerror('pcacov([1 2 3])', sprintf(_('%s must be a square matrix.'), 'V'));
asserts.checkerror('pcacov([1 2; 3 4])', sprintf(_('%s must be symmetric.'), 'V'));
asserts.checkerror('pcacov([1 0; 0 -1])', _('V must be positive semidefinite.'));
asserts.checkerror('pcacov([1 NaN; NaN 1])', sprintf(_('%s must contain finite values.'), 'V'));
%=============================================================================
