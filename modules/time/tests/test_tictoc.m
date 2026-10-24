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
asserts.isequal(nargin('tic'), 0);
asserts.isequal(nargout('tic'), 0);
asserts.isequal(nargin('toc'), 0);
asserts.isequal(nargout('toc'), 1);
%=============================================================================
tic();
sleep(0.2);
t = toc();
asserts.isapprox(t, 0.2, 0.15);
tic();
sleep(0.2)
t = toc();
asserts.isapprox(t, 0.2, 0.15);
sleep(0.2)
t = toc();
asserts.isapprox(t, 0.4, 0.2);
%=============================================================================
tic();
msg = evalc('toc()');
msgPattern = _('Elapsed time is {0:.6f} seconds.');
asserts.istrue(contains(msg, msgPattern(1:strfind(msgPattern, '{0') - 1)));
asserts.istrue(contains(msg, msgPattern(strfind(msgPattern, '}') + 1:end)));
%=============================================================================
