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
asserts.isequal(nargin('betalike'), 2);
asserts.isequal(nargout('betalike'), 2);
%=============================================================================
x = [0.12 0.2 0.35 0.5 0.7 0.85];
[NLOGL, AVAR] = betalike([1.5 1.8], x);
asserts.isapprox(NLOGL, -0.4170559701508001, 1e-12);
asserts.isapprox(AVAR, [0.6525003012710696 0.6015028591774459;
                       0.6015028591774459 0.9891159349410579], 1e-12);
%=============================================================================
[NLOGL, AVAR] = betalike(single([1.5 1.8]), single([0.2 0.4 0.6]));
asserts.isequal(class(NLOGL), 'double');
asserts.isequal(class(AVAR), 'double');
asserts.isapprox(NLOGL, -0.6984089533169221, 1e-12);
asserts.isapprox(AVAR, [1.305000605439844 1.203005681254352;
                       1.203005681254352 1.978231755770069], 1e-12);
%=============================================================================
[NLOGL, AVAR] = betalike([0 1], x);
asserts.istrue(isinf(NLOGL));
asserts.istrue(all(isnan(AVAR(:))));
[NLOGL, AVAR] = betalike([NaN 1], x);
asserts.istrue(isnan(NLOGL));
asserts.istrue(all(isnan(AVAR(:))));
[NLOGL, AVAR] = betalike([1 1], [0.2 NaN 0.6]);
asserts.istrue(isnan(NLOGL));
asserts.istrue(all(isnan(AVAR(:))));
%=============================================================================
asserts.checkerror('betalike([1 2])', _('Not enough input arguments.'));
asserts.checkerror('betalike({1 2}, [0.2 0.5])', sprintf(_('PARAMS must be a %s vector.'), 'two-element real numeric'));
asserts.checkerror('betalike([1 2 3], [0.2 0.5])', sprintf(_('PARAMS must be a %s vector.'), 'two-element real numeric'));
asserts.checkerror('betalike([1 2], {0.2 0.5})', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('betalike([1 2], [])', sprintf(_('%s must be nonempty.'), 'X'));
asserts.checkerror('betalike([1 2], [])', sprintf(_('%s must be nonempty.'), 'X'), 'Nelson:validators:mustBeNonempty');
asserts.checkerror('betalike([1 2], [0 0.2])', _('X must contain finite values in the open interval (0,1).'));
asserts.checkerror('betalike([1 2], [0.2 1])', _('X must contain finite values in the open interval (0,1).'));
asserts.checkerror('betalike([1 2], [0.2 Inf])', _('X must contain finite values in the open interval (0,1).'));
%=============================================================================