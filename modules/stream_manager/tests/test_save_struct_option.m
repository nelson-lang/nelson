%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% -struct saves the fields of a scalar structure as variables
R.a = 1;
R.b = 'x';
R.c = [1 2];
extensions = {'.mat', '.nh5'};
for k = 1:numel(extensions)
  f = [tempname(), extensions{k}];
  save(f, '-struct', 'R');
  s = load(f);
  assert_isequal(s, R);
  % listed names are fields, also when they come before -struct
  save(f, '-struct', 'R', 'a', 'c');
  s = load(f);
  assert_isequal(sort(fieldnames(s)), {'a'; 'c'});
  save(f, 'a', '-struct', 'R');
  s = load(f);
  assert_isequal(fieldnames(s), {'a'});
  save(f, '-struct', 'R', 'c');
  save(f, '-struct', 'R', 'a', '-append');
  s = load(f);
  assert_isequal(sort(fieldnames(s)), {'a'; 'c'});
  assert_isequal(s.c, [1 2]);
  delete(f);
end
% fields do not leak into the workspace
assert_isfalse(exist('a', 'var') == 1);
%=============================================================================
f = [tempname(), '.mat'];
x = 5;
S2 = struct('a', {1, 2});
msg = _('-struct expects the name of a scalar structure variable.');
assert_checkerror('save(f, ''-struct'', ''x'')', msg, 'Nelson:save:structArgNotScalarStruct');
assert_checkerror('save(f, ''-struct'', ''nope'')', msg, 'Nelson:save:structArgNotScalarStruct');
assert_checkerror('save(f, ''-struct'', ''S2'')', msg, 'Nelson:save:structArgNotScalarStruct');
msg = sprintf(_('Variable ''%s'' has no field named ''%s''.'), 'R', 'zz');
assert_checkerror('save(f, ''-struct'', ''R'', ''zz'')', msg, 'Nelson:save:fieldNotFoundInStruct');
msg = _('-struct must be followed by the name of a scalar structure variable.');
assert_checkerror('save(f, ''-struct'')', msg, 'Nelson:save:missingStructArg');
msg = _('Only one structure can be saved with -struct.');
assert_checkerror('save(f, ''-struct'', ''R'', ''-struct'', ''R'')', msg, 'Nelson:save:structSourcesExclusive');
%=============================================================================
