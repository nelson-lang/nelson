%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
d = dictionary([1 2], [3 4]);
asserts.isequal(class(d), 'dictionary');
asserts.istrue(isa(d, 'dictionary'));
asserts.istrue(isobject(d));
asserts.isequal(size(d), [1 1]);
asserts.isequal(numel(d), 1);
asserts.isequal(length(d), 1);
asserts.isequal(properties(d), {});
asserts.checkerror('struct(d)', _('Conversion to struct from dictionary is not possible.'));
asserts.checkerror('[d d]', _("Combining 'dictionary' objects is not allowed as they are designed to be scalar."));
asserts.checkerror('[d; d]', _("Combining 'dictionary' objects is not allowed as they are designed to be scalar."));
%=============================================================================
m = methods(d);
asserts.istrue(any(strcmp(m, 'dictionary')));
asserts.istrue(any(strcmp(m, 'entries')));
asserts.istrue(any(strcmp(m, 'lookup')));
%=============================================================================
d = dictionary([1 2], [3 4]);
% indexing a dictionary dispatches to its subsref (key lookup): a char key that
% is not present reports a missing key rather than an array-index error
asserts.checkerror('d(''1'')', 'Unknown key');
asserts.isequal(d("1"), 3);
d = dictionary(string({'abc'}), 5);
asserts.isequal(d('abc'), 5);
%=============================================================================
classdef_test_path = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(classdef_test_path);
p1 = ClassdefPoint(1, 2);
p2 = ClassdefPoint(3, 4);
d = dictionary([p1 p2], ["first" "second"]);
asserts.istrue(isKey(d, p1));
asserts.isequal(lookup(d, p2), "second");
%=============================================================================
d = dictionary(string({'a', 'b'}), {1, [2 3]});
asserts.isequal(d{'a'}, 1);
asserts.isequal(d{'b'}, [2 3]);
[a, b] = d{string({'a', 'b'})};
asserts.isequal(a, 1);
asserts.isequal(b, [2 3]);
d{'c'} = [4 5];
asserts.isequal(d('c'), {[4 5]});
asserts.isequal(d{'c'}, [4 5]);
d{'c'} = [];
asserts.istrue(isKey(d, 'c'));
asserts.isequal(d('c'), {[]});
asserts.isequal(d{'c'}, []);
d('c') = [];
asserts.isfalse(isKey(d, 'c'));
%=============================================================================
d = dictionary(string({'array', 'struct', 'cell'}), {[1 2 3], struct('field', [4 5 6]), {10, 20}});
asserts.isequal(d{'array'}(2), 2);
d{'array'}(2) = 22;
asserts.isequal(d{'array'}, [1 22 3]);
d{'array'}(end) = [];
asserts.isequal(d{'array'}, [1 22]);
asserts.isequal(d{'struct'}.field(3), 6);
d{'struct'}.field(2) = 55;
asserts.isequal(d{'struct'}.field, [4 55 6]);
asserts.isequal(d{'cell'}{2}, 20);
d{'cell'}{2} = 200;
asserts.isequal(d{'cell'}, {10, 200});
asserts.isequal(d('cell'), {{10, 200}});
%=============================================================================
d = dictionary();
d{string({'a', 'b'})} = {1, [2 3]};
asserts.isequal(d{'a'}, 1);
asserts.isequal(d{'b'}, [2 3]);
d{string({'a', 'b'})} = [10 20];
asserts.isequal(d{'a'}, 10);
asserts.isequal(d{'b'}, 20);
d{string({'a', 'b'})} = 7;
asserts.isequal(d{'a'}, 7);
asserts.isequal(d{'b'}, 7);
%=============================================================================
d = dictionary();
d{'a'} = 1;
asserts.istrue(isConfigured(d));
[keyType, valueType] = types(d);
asserts.isequal(keyType, "string");
asserts.isequal(valueType, "cell");
asserts.isequal(d('a'), {1});
asserts.isequal(d{'a'}, 1);
%=============================================================================
d = dictionary("a", 1);
asserts.checkerror('d{"a"}', sprintf(_("Using curly braces on a dictionary with '%s' value type is not supported. The dictionary value type must be 'cell'."), 'double'));
%=============================================================================
