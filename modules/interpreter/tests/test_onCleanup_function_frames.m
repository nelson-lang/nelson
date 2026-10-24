%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% onCleanup attaches to the frame that creates it: its task runs when that
% frame exits, whatever kind of function the frame belongs to.
addpath([modulepath('interpreter', 'tests'), '/onCleanup']);
%=============================================================================
o = OcProbe();
R = evalc('o.instanceMethod(); disp(''after call'');');
REF = sprintf('instance body\ninstance cleanup\nafter call\n');
asserts.isequal(R, REF);
%=============================================================================
R = evalc('OcProbe.staticMethod(); disp(''after call'');');
REF = sprintf('static body\nstatic cleanup\nafter call\n');
asserts.isequal(R, REF);
%=============================================================================
R = evalc('try, o.failingMethod(); catch ex, disp(ex.identifier); end');
REF = sprintf('failing cleanup\nOcProbe:failure\n');
asserts.isequal(R, REF);
%=============================================================================
R = evalc('oncleanup_localfunction_probe();');
REF = sprintf('main start\nlocal body\nlocal cleanup\nmain end\n');
asserts.isequal(R, REF);
%=============================================================================
% each recursion level owns its own task: an inner frame must not run the
% tasks of the outer ones.
R = evalc('oncleanup_recursion_probe(3);');
REF = sprintf('end 1\ncleanup 1\nend 2\ncleanup 2\nend 3\ncleanup 3\n');
asserts.isequal(R, REF);
%=============================================================================
