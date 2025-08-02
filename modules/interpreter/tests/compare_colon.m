%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function R = compare_colon(nb_test, varargin)
  R = false;
  switch(nb_test)
    case 1
      REF = {1, 4, 7, 2, 5, 8, 3, 6, 9};
      R = isequal(varargin, REF);
    case 2
      REF = {1, 2, 3};
      R = isequal(varargin, REF);
    case 3
      REF = {2, 5, 8};
      R = isequal(varargin, REF);
    case 4
      REF = {1, 4, 7, 2, 5, 8, 3, 6, 9};
      R = isequal(varargin, REF);
    case 5
      REF = {1, 4, 7, 2, 5, 8, 3, 6, 9};
      R = isequal(varargin, REF);
    otherwise
      R = false;
    end
  end
