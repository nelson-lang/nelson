%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A.f = B, A.f.g = B, A.f{I} = B, A(I).f = B on a non-empty value that is
% neither a struct, an object nor a handle
%=============================================================================
msg = _('Unable to perform assignment because dot indexing is not supported for variables of this type.');
%=============================================================================
x = 5;
ME = [];
try
  x.b = 1;
catch ME
end
asserts.isfalse(isempty(ME));
asserts.isequal(ME.message, msg);
asserts.isequal(ME.identifier, 'Nelson:structAssToNonStruct');
asserts.isequal(x, 5);
%=============================================================================
s.a = [1 2];
ME = [];
try
  s.a.b = 1;
catch ME
end
asserts.isfalse(isempty(ME));
asserts.isequal(ME.message, msg);
asserts.isequal(ME.identifier, 'Nelson:structAssToNonStruct');
asserts.isequal(s.a, [1 2]);
%=============================================================================
s = 5;
ME = [];
try
  s(2).a = 1;
catch ME
end
asserts.isfalse(isempty(ME));
asserts.isequal(ME.identifier, 'Nelson:structAssToNonStruct');
asserts.isequal(s, 5);
%=============================================================================
codes = {'x = 5; x.b.c = 1;', 'x = 5; x.b{1} = 1;', 'x = 5; x.b(2) = 1;', ...
  'x = 5; x.b(1).c = 1;', 's.a = [1 2]; s.a.b.c = 1;', 's.a = [1 2]; s.a.b(2) = 1;', ...
  's.a = [1 2]; s.a.b{1} = 1;', 's.a = @sin; s.a.b{1} = 1;', 's.a = {1}; s.a.b{1} = 1;', ...
  's.a = ''ab''; s.a.b{1} = 1;', 's = [1 2]; s(1).a = 1;', 'x = {1}; x.b = 1;', ...
  'x = {1}; x(3).b = 1;', 'x = true; x.b = 1;', 'x = int8(3); x.b = 1;', ...
  'x = sparse(1); x.b = 1;', 'x = 5; x(3).b.c = 1;', 'c = {5}; c{1}.b = 1;', ...
  'x = 5; [x.b] = deal(1);', 'x = 5; x.(''b'') = 1;', 's.a = 5; s.a.(''b'') = 1;', ...
  's(2).a = 5; s(2).a.b = 1;'};
for k = 1:numel(codes)
  asserts.isequal(dotAssignErrorIdentifier(codes{k}), 'Nelson:structAssToNonStruct');
end
%=============================================================================
% empty values still become structs
s = [];
s.a = [];
s.a.b{1} = 1;
asserts.isequal(class(s.a), 'struct');
asserts.isequal(s.a.b, {1});
x = [];
x.b{1} = 1;
asserts.isequal(class(x), 'struct');
x = '';
x.b = 1;
asserts.isequal(class(x), 'struct');
x = [];
x(2).b = 1;
asserts.isequal(size(x), [1 2]);
%=============================================================================
function id = dotAssignErrorIdentifier(code)
  id = '';
  try
    eval(code);
  catch ME
    id = ME.identifier;
  end
end
%=============================================================================
