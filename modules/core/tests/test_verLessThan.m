%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('verLessThan'), 2);
assert_isequal(nargout('verLessThan'), 1);
%=============================================================================
% Return type is always a logical scalar.
r = verLessThan('matlab', '9.1');
assert_istrue(islogical(r));
assert_istrue(isscalar(r));
%=============================================================================
% The reserved pseudo-toolbox reports a modern release: historical (old) gates
% are all false, so toolboxes take their current code paths.
assert_isequal(verLessThan('matlab', '7.9'), false); % requires a recent-enough release
assert_isequal(verLessThan('matlab', '9.1'), false);
assert_isequal(verLessThan('matlab', '9.14'), false);
assert_isequal(verLessThan('matlab', '24.2'), false);
assert_isequal(verLessThan('matlab', '25.1'), false); % equal is not less-than
assert_isequal(verLessThan('matlab', '99.0'), true); % a truly newer release
%=============================================================================
% Generic toolbox path stays honest: it uses ver(TOOLBOX).Version.
entry = ver('Nelson');
installed = entry(1).Version;
assert_isequal(verLessThan('Nelson', installed), false); % equal
assert_isequal(verLessThan('Nelson', '9999.0'), true); % clearly newer
assert_isequal(verLessThan('Nelson', '0.1'), false); % clearly older
%=============================================================================
% Comparison is numeric field by field, not lexicographic (2.10 > 2.9).
assert_isequal(verLessThan('Nelson', '2.10'), true);
assert_isequal(verLessThan('Nelson', '2.9'), true);
%=============================================================================
% String scalar inputs are accepted.
assert_isequal(verLessThan("matlab", "7.9"), false);
%=============================================================================
% Malformed version string errors.
assert_checkerror('verLessThan(''matlab'', ''abc'')', _('The VERSION string must be of the form ''major[.minor[.revision]]'', such as ''7.0.1''.'));
assert_checkerror('verLessThan(''matlab'', ''9.'')', _('The VERSION string must be of the form ''major[.minor[.revision]]'', such as ''7.0.1''.'));
assert_checkerror('verLessThan(''matlab'', ''9.x'')', _('The VERSION string must be of the form ''major[.minor[.revision]]'', such as ''7.0.1''.'));
%=============================================================================
% Unknown / not-installed toolbox errors.
assert_checkerror('verLessThan(''NoSuchToolbox'', ''1.0'')', _('The toolbox ''NoSuchToolbox'' is not installed.'));
%=============================================================================
% Wrong argument types error.
assert_checkerror('verLessThan(1, ''1.0'')', sprintf(_('Wrong type for argument #%d: %s expected.'), 1, 'string'));
assert_checkerror('verLessThan(''matlab'', 2)', sprintf(_('Wrong type for argument #%d: %s expected.'), 2, 'string'));
%=============================================================================
% Wrong number of inputs errors.
assert_checkerror('verLessThan(''matlab'')', _('Not enough input arguments.'));
%=============================================================================
