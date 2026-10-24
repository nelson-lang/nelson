%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = odeIsDefaultSolverOptions(obj)
  names = properties(obj);
  tf = true;
  for k = 1:length(names)
    name = names{k};
    value = obj.(name);
    switch name
      case {'InitialStep', 'MaxStep', 'MinStep', 'OutputFcn', 'OutputSelection'}
        tf = tf && isempty(value);
      case 'NormControl'
        tf = tf && strcmpi(char(value), 'off');
      case 'Vectorization'
        if iscell(value)
          tf = tf && strcmpi(char(value{1}), 'off') && strcmpi(char(value{2}), 'off');
        else
          tf = tf && strcmpi(char(value), 'off');
        end
      case 'BDF'
        tf = tf && strcmpi(char(value), 'off');
      case 'MaxOrder'
        if isprop(obj, 'ID') && strcmp(obj.ID, 'cvodesnonstiff')
          tf = tf && isequal(value, 12);
        else
          tf = tf && isequal(value, 5);
        end
      case 'ComputeConsistentInitialConditions'
        tf = tf && isequal(value, true);
    end
  end
end
%=============================================================================
