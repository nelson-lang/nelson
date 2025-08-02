%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [status, pathcmake] = findcmake()
  status = false;
  pathcmake = '';
  if ispc()
    embedded = [nelsonroot(), '/tools/cmake/bin/cmake.exe'];
    if isfile(embedded)
      status = true;
      pathcmake = embedded;
      return;
    end
  end
  if ispc()
    cmake_name = 'cmake.exe';
  else
    cmake_name = 'cmake';
  end
  r = searchenv(cmake_name,'PATH');
  if ~isempty(r)
    status = true;
    pathcmake = r{1};
  end
end
%=============================================================================
