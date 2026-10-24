%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% T.name{k} = v and T.(name){k} = v on a numeric table variable
%=============================================================================
msg = _('Unable to perform assignment because brace indexing is not supported for variables of this type.');
T = table([1; 2], [3; 4], 'VariableNames', {'A', 'B'});
%=============================================================================
ME = [];
try
  T.B{2} = 1;
catch ME
end
asserts.isfalse(isempty(ME));
asserts.isequal(ME.message, msg);
asserts.isequal(ME.identifier, 'Nelson:cellAssToNonCell');
%=============================================================================
ME = [];
s = 'B';
try
  T.(s){2} = 1;
catch ME
end
asserts.isfalse(isempty(ME));
asserts.isequal(ME.message, msg);
asserts.isequal(ME.identifier, 'Nelson:cellAssToNonCell');
%=============================================================================
asserts.isequal(T.B, [3; 4]);
%=============================================================================
T = table({1; 2}, 'VariableNames', {'C'});
T.C{2} = 5;
asserts.isequal(T.C, {1; 5});
%=============================================================================
