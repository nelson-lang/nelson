%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('nargchk'), 4);
assert_isequal(nargout('nargchk'), 1);
%=============================================================================
% Not enough input arguments: n < low.
msg = nargchk(2, 3, 1);
assert_istrue(ischar(msg));
assert_isfalse(isempty(msg));
assert_isequal(msg, _('Not enough input arguments.'));
%=============================================================================
% Too many input arguments: n > high.
msg = nargchk(1, 2, 3);
assert_istrue(ischar(msg));
assert_isfalse(isempty(msg));
assert_isequal(msg, _('Too many input arguments.'));
%=============================================================================
% In range: empty char row vector.
msg = nargchk(1, 3, 2);
assert_istrue(ischar(msg));
assert_istrue(isempty(msg));
assert_isequal(msg, '');
%=============================================================================
% Boundary values are inclusive.
assert_isequal(nargchk(1, 3, 1), '');
assert_isequal(nargchk(1, 3, 3), '');
%=============================================================================
% struct output form: in range returns an empty struct.
s = nargchk(2, 3, 2, 'struct');
assert_istrue(isstruct(s));
assert_istrue(isempty(s));
assert_isequal(fieldnames(s), {'message'; 'identifier'});
%=============================================================================
% struct output form: out of range returns a populated struct.
s = nargchk(2, 3, 1, 'struct');
assert_istrue(isstruct(s));
assert_isequal(numel(s), 1);
assert_isequal(s.message, _('Not enough input arguments.'));
assert_isequal(s.identifier, 'Nelson:nargchk:notEnoughInputs');
%=============================================================================
% string scalar output type is accepted.
assert_isequal(nargchk(1, 3, 2, "string"), '');
%=============================================================================
% error(nargchk(...)) raises when out of range and is a no-op in range.
assert_checkerror('error(nargchk(2, 3, 1))', _('Not enough input arguments.'));
error(nargchk(1, 3, 2));
%=============================================================================
% Too few / too many arguments to nargchk itself.
assert_checkerror('nargchk(1, 2)', _('Not enough input arguments.'));
assert_checkerror('nargchk(1, 2, 3, ''struct'', 5)', _('Too many input arguments.'));
%=============================================================================
