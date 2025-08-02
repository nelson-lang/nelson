%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
msg = _('valid JSON Object expected.')
assert_checkerror("r = jsondecode('');", msg);
%=============================================================================
r = jsondecode('NaN');
ref = NaN;
assert_isequal(r, ref);
%=============================================================================
r = jsondecode('  NaN ');
ref = NaN;
assert_isequal(r, ref);
%=============================================================================
r = jsondecode('Inf');
ref = Inf;
assert_isequal(r, ref);
%=============================================================================
r = jsondecode('-Inf');
ref = -Inf;
assert_isequal(r, ref);
%=============================================================================
r = jsondecode('null');
ref = [];
assert_isequal(r, ref);
%=============================================================================
r = jsondecode('true');
ref = true;
assert_isequal(r, ref);
%=============================================================================
r = jsondecode('false');
ref = false;
assert_isequal(r, ref);
%=============================================================================
r = jsondecode(' 444 ');
ref = 444;
assert_isequal(r, ref);
%=============================================================================
r = jsondecode('     3.14');
ref = pi;
assert_isapprox(r, ref,1e-2);
%=============================================================================
r = jsondecode(' "rrr"');
ref = 'rrr';
assert_isequal(r, ref);
%=============================================================================
r = jsondecode('{}');
ref = struct();
assert_isequal(r, ref);
%=============================================================================
% Array of double ie : matrix
r = jsondecode('[1.4]');
ref = [1.4];
assert_isequal(r, ref);
%=============================================================================
r = jsondecode('[1.4, 4]');
ref = [1.4; 4];
assert_isequal(r, ref);
%=============================================================================
r = jsondecode('[[1,2],[3,4]]');
ref = [1, 2; 3, 4];
assert_isequal(r, ref);
%=============================================================================
r = jsondecode('[[[1,2],[3,4]],[[5,6],[7,8]]]');
ref = reshape([1     5     3     7     2     6     4     8], 2, 2, 2);
assert_isequal(r, ref);
%=============================================================================
% Array of logical ie : matrix
r = jsondecode('[true]');
ref = true;
assert_isequal(r, ref);
%=============================================================================
r = jsondecode('[true, false]');
ref = [true; false];
assert_isequal(r, ref);
%=============================================================================
r = jsondecode('[[true,false],[false,true]]');
ref = [true, false; false, true];
assert_isequal(r, ref);
%=============================================================================
r = jsondecode('[[[true,false],[false,true]],[[true,false],[false,true]]]');
ref = reshape([true   true   false  false  false  false  true   true], 2, 2, 2);
assert_isequal(r, ref);
%=============================================================================
% Array of string ie : cell of strings
r = jsondecode('["Nelson"]');
ref = {'Nelson'};
assert_isequal(r, ref);
%=============================================================================
r = jsondecode('["Nelson", "interpreter"]');
ref = {'Nelson'; 'interpreter'};
assert_isequal(r, ref);
%=============================================================================
% struct
r = jsondecode('{"a":1}');
ref = struct('a', 1);
assert_isequal(r, ref);
%=============================================================================
r = jsondecode('[{"a":1},{"a":2}]');
assert_isequal(r(1).a, 1);
assert_isequal(r(2).a, 2);
%=============================================================================
r = jsondecode('[[{"a":1},{"a":2}],[{"a":3},{"a":4}]]');
assert_isequal(r(1,1).a, 1);
assert_isequal(r(1,2).a, 2);
assert_isequal(r(2,1).a, 3);
assert_isequal(r(2,2).a, 4);
%=============================================================================
r = jsondecode('["one", 2, "three"]');
ref = {'one'; 2; 'three'};
assert_isequal(r, ref);
%=============================================================================
r = jsondecode('{"a":1,"b":"NaN"}');
assert_isequal(r.a, 1);
assert_isequal(r.b, NaN);
%=============================================================================
r = jsondecode('[{"toto":42, "tuto":44, "toto":45, "tuto":46, "tuto":47, , "toto":48}]');
assert_isequal(r.toto, 42);
assert_isequal(r.tuto, 44);
assert_isequal(r.toto_1, 45);
assert_isequal(r.tuto_1, 46);
assert_isequal(r.toto_2, 48);
assert_isequal(r.tuto_2, 47);
%=============================================================================
r = jsondecode('[{"a":43, "b":44, "c":45, "d":46}]');
assert_isequal(r.a, 43);
assert_isequal(r.b, 44);
assert_isequal(r.c, 45);
assert_isequal(r.d, 46);
%=============================================================================
r = jsondecode('[{"":42}]');
assert_isequal(r.x, 42);
%=============================================================================
r = jsondecode('[{"_":42}]');
assert_isequal(r.x_, 42);
%=============================================================================
r = jsondecode('[{"3":42}]');
assert_isequal(r.x_3, 42);
%=============================================================================
r = jsondecode('[{"漢字":42}]');
assert_isequal(r.x__, 42);
%=============================================================================
r = jsondecode('[{"''":42}]');
assert_isequal(r.x_, 42);
%=============================================================================
json = fileread([modulepath('json', 'tests'), '/example1.json']);
r = jsondecode(json);
assert_isequal(fieldnames(r), {'glossary'});
%=============================================================================
json = fileread([modulepath('json', 'tests'), '/example2.json']);
r = jsondecode(json);
assert_isequal(fieldnames(r), {'quiz'});
assert_isequal(fieldnames(r.quiz), {'sport';'maths'});
%=============================================================================
r = jsondecode('{"names": [[1], [2]]}');
assert_isequal(r.names, [1; 2]);
%=============================================================================
r = jsondecode('{"names": [[], []]}');
assert_isequal(r.names, {[]; []});
%=============================================================================
r = jsondecode('{"names": [[], [2]]}');
assert_isequal(r.names, {[]; [2]});
%=============================================================================
r = jsondecode("[1, null]");
ref = [1; NaN];
assert_isequal(r, ref);
%=============================================================================
ref = struct();
r = jsondecode("[true, null]");
ref = {true; []};
assert_isequal(r, ref);
%=============================================================================
ref = struct();
r = jsondecode('{"a":null}');
ref.a = [];
assert_isequal(r, ref);
%=============================================================================
ref = struct();
r = jsondecode('{"a":[1,null]}');
ref.a = [1; NaN];
assert_isequal(r, ref);
%=============================================================================
