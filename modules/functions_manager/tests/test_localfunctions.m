%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
asserts.isequal(nargin('localfunctions'), 0);
asserts.isequal(nargout('localfunctions'), 1);
%=============================================================================
R = evalin('base', 'localfunctions()');
asserts.isequal(R, {});
%=============================================================================
asserts.checkerror('localfunctions(1)', _('Too many input arguments.'));
asserts.checkerror('[a, b] = localfunctions()', _('Too many output arguments.'));
%=============================================================================
R = localfunctions_fixture();
asserts.istrue(iscell(R));
asserts.isequal(size(R), [3 1]);
asserts.istrue(isfunction_handle(R{1}));
asserts.istrue(isfunction_handle(R{2}));
asserts.istrue(isfunction_handle(R{3}));
names = {func2str(R{1}); func2str(R{2}); func2str(R{3})};
asserts.isequal(names, {'local_one'; 'local_two'; 'call_from_local'});
asserts.isequal(R{1}(3), 4);
asserts.isequal(R{2}(3), 6);
%=============================================================================
R = localfunctions_fixture('from_local');
names = {func2str(R{1}); func2str(R{2}); func2str(R{3})};
asserts.isequal(names, {'local_one'; 'local_two'; 'call_from_local'});
%=============================================================================
R = localfunctions_nested_fixture();
names = {func2str(R{1})};
asserts.isequal(size(R), [1 1]);
asserts.isequal(names, {'visible_local'});
asserts.isequal(R{1}(7), 6);
%=============================================================================
clear R;
localfunctions_empty_script_fixture;
asserts.isequal(R, {});
%=============================================================================
clear R;
localfunctions_script_fixture;
asserts.isequal(size(R), [2 1]);
names = {func2str(R{1}); func2str(R{2})};
asserts.isequal(names, {'script_local_one'; 'script_local_two'});
asserts.isequal(R{1}(10), 9);
asserts.isequal(R{2}(10), 12);
%=============================================================================
obj = ClassdefLocalfunctionsFixture();
R = obj.localHandles();
asserts.isequal(size(R), [2 1]);
names = {func2str(R{1}); func2str(R{2})};
asserts.isequal(names, {'classdef_local_one'; 'classdef_local_two'});
asserts.isequal(R{1}(5), 15);
asserts.isequal(R{2}(5), 25);
asserts.isequal(obj.callFirstLocal(5), 15);
asserts.isequal(ClassdefLocalfunctionsFixture.staticLocalNames(), names);
%=============================================================================
asserts.isequal(evalin('base', 'localfunctions()'), {});
handles = localfunctions_fixture();
asserts.isequal(size(handles), [3 1]);
names = {func2str(handles{1}); func2str(handles{2}); func2str(handles{3})};
asserts.isequal(names, {'local_one'; 'local_two'; 'call_from_local'});
asserts.isequal(handles{1}(3), 4);
asserts.isequal(handles{2}(3), 6);
nestedHandles = localfunctions_nested_fixture();
asserts.isequal(size(nestedHandles), [1 1]);
asserts.isequal(func2str(nestedHandles{1}), 'visible_local');
asserts.isequal(nestedHandles{1}(7), 6);
asserts.checkerror('localfunctions(1)', _('Too many input arguments.'));
asserts.checkerror('[a, b] = localfunctions()', _('Too many output arguments.'));
%=============================================================================
