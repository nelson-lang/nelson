%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
line = 'id=12345; name=alpha_beta; status=OK; value=-42.75; ';
txt = repmat(line, 1, 10000);

tic(); S = regexp(txt, '\d+', 'start'); toc()
assert_isequal(S(1), 4);

tic(); M = regexp(txt, 'name=([A-Za-z_]+); status=([A-Z]+)', 'tokens'); toc()
assert_isequal(M{1}{1}, 'alpha_beta');
assert_isequal(M{1}{2}, 'OK');

tic(); N = regexp(txt, 'value=(?<number>[-+]?\d+(?:\.\d+)?)', 'names'); toc()
assert_isequal(N(1).number, '-42.75');

tic(); R = regexp(txt, 'status=OK', 'match', 'once'); toc()
assert_isequal(R, 'status=OK');
%=============================================================================
