%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function classname = report_caseClassname(testCase)
  classname = '';
  if isfield(testCase, 'classname')
    classname = testCase.classname;
  elseif isfield(testCase, 'module')
    classname = testCase.module;
  end
end
