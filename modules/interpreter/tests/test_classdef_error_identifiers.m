%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% Classdef structural errors carry a stable identifier so user code can catch
% them programmatically.
%=============================================================================
path_test = [modulepath('interpreter', 'tests'), '/classdef'];
addpath(path_test);
%=============================================================================
id = '';
try
  ClassdefAbstract();
catch me
  id = me.identifier;
end
asserts.isequal(id, 'Nelson:class:abstract');
%=============================================================================
id = '';
try
  ClassdefSealedSubclass();
catch me
  id = me.identifier;
end
asserts.isequal(id, 'Nelson:class:sealed');
%=============================================================================
id = '';
try
  ClassdefDuplicateProperty();
catch me
  id = me.identifier;
end
asserts.isequal(id, 'Nelson:class:duplicateProperty');
%=============================================================================
id = '';
try
  ClassdefDuplicateMethod();
catch me
  id = me.identifier;
end
asserts.isequal(id, 'Nelson:class:duplicateMethod');
%=============================================================================
id = '';
try
  ClassdefConflictPropertyChild();
catch me
  id = me.identifier;
end
asserts.isequal(id, 'Nelson:class:inheritanceConflict');
%=============================================================================
id = '';
try
  ClassdefConflictMethodChild();
catch me
  id = me.identifier;
end
asserts.isequal(id, 'Nelson:class:inheritanceConflict');
%=============================================================================
rmpath(path_test);
%=============================================================================
