%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function show(obj)
  if isa(obj, 'optim.problemdef.OptimizationVariable')
    showVariable(obj);
    return
  end
  if isa(obj, 'optim.problemdef.OptimizationExpression')
    showExpression(obj);
    return
  end
  if isa(obj, 'optim.problemdef.OptimizationConstraint')
    showConstraint(obj);
    return
  end
  if isa(obj, 'optim.problemdef.OptimizationProblem')
    showProblem(obj);
    return
  end
  disp(obj)
end
%=============================================================================
function showExpression(obj)
  disp(['  ', obj.Display])
end
%=============================================================================
function showConstraint(obj)
  disp(['  ', obj.Left.Display, ' ', obj.Relation, ' ', obj.Right.Display])
end
%=============================================================================
function showProblem(obj)
  disp('  OptimizationProblem : ')
  disp(' ')
  names = objectiveVariables(obj);
  disp('	Solve for:')
  for k = 1:length(names)
    disp(['       ', names{k}])
  end
  disp(' ')
  disp([char(9), lower(obj.ObjectiveSense), ' :'])
  if isempty(obj.Objective)
    disp('       []')
  else
    disp(['       ', obj.Objective.Display])
  end
  constraints = constraintList(obj);
  if ~isempty(constraints)
    disp(' ')
    disp(' ')
    disp('	subject to :')
    for k = 1:length(constraints)
      c = constraints{k};
      disp(['       ', c.Left.Display, ' ', c.Relation, ' ', c.Right.Display])
    end
  end
  showProblemBounds(obj, names);
end
%=============================================================================
function showProblemBounds(obj, names)
  if isempty(obj.Objective)
    return
  end
  lines = {};
  for k = 1:length(names)
    name = names{k};
    if isfield(obj.Objective.VarLowerBound, name)
      lb = obj.Objective.VarLowerBound.(name);
      if any(isfinite(lb(:)))
        lines{end + 1} = [name, ' >= ', boundText(lb)];
      end
    end
    if isfield(obj.Objective.VarUpperBound, name)
      ub = obj.Objective.VarUpperBound.(name);
      if any(isfinite(ub(:)))
        lines{end + 1} = [name, ' <= ', boundText(ub)];
      end
    end
  end
  if isempty(lines)
    return
  end
  disp(' ')
  disp(' ')
  disp('	variable bounds :')
  for k = 1:length(lines)
    disp(['       ', lines{k}])
  end
end
%=============================================================================
function txt = boundText(value)
  if isscalar(value)
    txt = num2str(value);
  else
    txt = ['[', int2str(size(value, 1)), 'x', int2str(size(value, 2)), ' double]'];
  end
end
%=============================================================================
function showVariable(obj)
  sz = double(obj.Size);
  if isempty(sz)
    sz = [1 1];
  end
  if numel(sz) == 1
    rows = sz(1);
    cols = 1;
  else
    rows = sz(1);
    cols = sz(2);
  end
  if prod(sz) == 1
    disp(['    [ ', obj.Name, ' ]'])
    return
  end
  for i = 1:rows
    line = '    ';
    for j = 1:cols
      if j > 1
        line = [line, '    '];
      end
      line = [line, '[ ', obj.Name, variableIndex(sz, i, j), ' ]'];
    end
    disp(line)
  end
end
%=============================================================================
function indexText = variableIndex(sz, rowIndex, columnIndex)
  if numel(sz) == 1 || (numel(sz) == 2 && min(sz) == 1)
    linearIndex = rowIndex + (columnIndex - 1) * sz(1);
    indexText = ['(', int2str(linearIndex), ')'];
  else
    indexText = ['(', int2str(rowIndex), ',', int2str(columnIndex), ')'];
  end
end
%=============================================================================
