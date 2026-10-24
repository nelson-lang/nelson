%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [A, B, C, D] = linmod(model, x0, u0)
  % linmod  Numerical linearization of an nflow model.
  %
  %   [A, B, C, D] = linmod(model)
  %   [A, B, C, D] = linmod(model, x0, u0)
  %
  %   model is a loaded system (name or handle) or the path to a .nflow file.
  %   x0 (length nx) and u0 (length nu) optionally set the operating point;
  %   absent, the post-INIT state and zero inputs are used.
  %
  %   Returns the continuous-time state-space linearization about the model's
  %   post-INIT operating point (external inputs held at zero), obtained by
  %   central-differencing the same global right-hand side the ode4 / CVODES
  %   solver assembles:
  %       A = d(xdot)/dx   (nx-by-nx)      B = d(xdot)/du   (nx-by-nu)
  %       C = dy/dx        (ny-by-nx)      D = dy/du        (ny-by-nu)
  %   where x is the packed continuous state, u the external-port Label Source
  %   inputs (isExternalPort), and y the Label Sink outputs. A linear model
  %   linearizes to itself. An input that should participate in B / D must be an
  %   external-port Label Source; internal goto/from routing does not count.
  %
  %   Both top-level (flat) models and continuous states nested inside
  %   subsystems are supported: a nested state's coupling to the external inputs
  %   (B) and to the outputs across the subsystem boundary (C) is captured.
  narginchk(1, 3);
  nargoutchk(0, 4);

  % Pass the model's raw JSON straight through: jsondecode/jsonencode would
  % homogenize heterogeneous (nested subsystem) block arrays and corrupt the
  % inner diagram, so operating-point vectors go as separate builtin arguments
  % and the engine injects any required scope sink itself.
  documentJson = prepare_model_json(linmod_model_json(model));
  if nargin >= 3 && ~isempty(u0)
    resultJson = __nflow_linearize__(documentJson, double(x0(:))', double(u0(:))');
  elseif nargin >= 2 && ~isempty(x0)
    resultJson = __nflow_linearize__(documentJson, double(x0(:))');
  else
    resultJson = __nflow_linearize__(documentJson);
  end
  r = jsondecode(resultJson);
  if ~isfield(r, 'status') || ~strcmp(r.status, 'done')
    reason = 'unknown error';
    if isfield(r, 'reason')
      reason = r.reason;
    end
    error('Nelson:nflow_engine:linmod', 'linmod failed: %s', reason);
  end

  nx = double(r.nx);
  ny = double(r.ny);
  nu = double(r.nu);
  A = linmod_reshape(r.A, nx, nx);
  B = linmod_reshape(r.B, nx, nu);
  C = linmod_reshape(r.C, ny, nx);
  D = linmod_reshape(r.D, ny, nu);
end
%=============================================================================
function M = linmod_reshape(flat, rows, cols)
  % Row-major flat vector -> rows-by-cols matrix.
  if rows * cols == 0
    M = zeros(rows, cols);
    return
  end
  M = reshape(flat(:), cols, rows).';
end
%=============================================================================
function documentJson = linmod_model_json(model)
  % A loaded model (handle or name) is snapshotted to a temp file; otherwise the
  % argument is treated as a .nflow file path.
  if isnumeric(model) && isscalar(model)
    documentJson = linmod_snapshot(model);
    return
  end
  if ~(ischar(model) || (isstring(model) && isscalar(model)))
    error('Nelson:nflow_engine:linmodArgs', 'model must be a name, a handle or a .nflow file path.');
  end
  name = char(model);
  if bdIsLoaded(name)
    documentJson = linmod_snapshot(name);
    return
  end
  filename = name;
  if ~isfile(filename)
    if ~endsWith(lower(filename), '.nflow')
      filename = [filename, '.nflow'];
    end
  end
  if ~isfile(filename)
    error('Nelson:nflow_engine:modelNotFound', ...
      'Model ''%s'' is not a loaded system nor an existing .nflow file.', name);
  end
  documentJson = fileread(filename);
end
%=============================================================================
function documentJson = linmod_snapshot(model)
  target = [tempname(), '.nflow'];
  cleanup = onCleanup(@() linmod_delete_snapshot(target));
  save_system(model, target);
  documentJson = fileread(target);
end
%=============================================================================
function linmod_delete_snapshot(target)
  if isfile(target)
    delete(target);
  end
end
%=============================================================================
