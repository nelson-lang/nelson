%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
currentpath = fileparts(nfilename('fullpathext'), 'path');
scriptpath = [currentpath, '/script_to_profile.m'];
%=============================================================================
profile('clear');
profile('on');
run(scriptpath);
profile('off');
p1 = profile('info');
%=============================================================================
profile('on');
run(scriptpath);
profile('off');
p2 = profile('info');
profile('clear');
%=============================================================================
p1 = p1(strcmp({p1.Filename}, scriptpath));
p2 = p2(strcmp({p2.Filename}, scriptpath));
%=============================================================================
asserts.isequal(length(p1), 6);
asserts.isequal(length(p2), 6);
asserts.isequal({p2.FunctionName}, {p1.FunctionName});
asserts.isequal([p2.NumCalls], [p1.NumCalls]);
%=============================================================================
