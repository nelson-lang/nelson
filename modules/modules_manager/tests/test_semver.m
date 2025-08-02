%=============================================================================
% Copyright (c) 2018 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('semver'), 2);
assert_isequal(nargout('semver'), 1);
%=============================================================================
assert_isequal(semver('1.5.10', '2.3.0'), -1);
assert_isequal(semver('2.3.0', '1.5.10'), 1);
assert_isequal(semver('1.5.10', '1.5.10'), 0);
assert_isequal(semver('1.2.3', '~1.2.3'), 1);
assert_isequal(semver('1.5.3', '~1.2.3'), 0);
assert_isequal(semver('1.0.3', '~1'), 1);
assert_isequal(semver('2.0.3', '~1'), 0);
assert_isequal(semver('1.2.3-alpha', '<1.2.3-beta'), 1);
assert_isequal(semver('1.2.3-alpha', '>1.2.3-beta'), 0);
assert_isequal(semver('1.2.3', '^1.2.3'), 1);
assert_isequal(semver('1.2.2', '^1.2.3'), 1);
assert_isequal(semver('1.9.9', '^1.2.3'), 1);
assert_isequal(semver('2.0.1', '^1.2.3'), 0);
%=============================================================================

