%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [x, u, y, dx] = trim(model, x0, u0)
  % trim  Find a steady-state (equilibrium) operating point of an nflow model.
  %
  %   [x, u, y, dx] = trim(model)
  %   [x, u, y, dx] = trim(model, x0, u0)
  %
  %   model is a loaded system (name or handle) or the path to a .nflow file.
  %   x0 (length nx) seeds the search; u0 (length nu) is the fixed input.
  %
  %   Solves f(x, u0) = 0 for the continuous state x by a Newton iteration on
  %   the state Jacobian A = d(xdot)/dx (from linmod), holding the inputs u = u0
  %   fixed. Returns the equilibrium state x, the inputs u, the outputs y at the
  %   solution, and dx = xdot there (its norm measures how close to equilibrium).
  narginchk(1, 3);
  nargoutchk(0, 4);

  % Pass the model's raw JSON straight through (jsondecode/jsonencode would
  % homogenize heterogeneous nested-subsystem block arrays and corrupt the inner
  % diagram); operating-point vectors go as separate builtin arguments and the
  % engine injects any required scope sink itself.
  documentJson = prepare_model_json(trim_model_json(model));

  % Probe the dimensions at the default operating point.
  r = trim_linearize(documentJson, [], []);
  nx = double(r.nx);
  nu = double(r.nu);

  if nargin >= 2 && ~isempty(x0)
    x = double(x0(:));
  else
    x = zeros(nx, 1);
  end
  if nargin >= 3 && ~isempty(u0)
    u = double(u0(:));
  else
    u = zeros(nu, 1);
  end

  tol = 1e-9;
  maxit = 50;
  dx = zeros(nx, 1);
  y = zeros(double(r.ny), 1);
  for it = 1:maxit
    r = trim_linearize(documentJson, x(:)', u(:)');
    dx = r.f0(:);
    y = r.y0(:);
    if nx == 0 || norm(dx) < tol
      break
    end
    A = reshape(r.A(:), nx, nx).';
    step = A \ dx;
    if any(~isfinite(step))
      error('Nelson:nflow_engine:trimSingularJacobian', ...
        'trim: singular state Jacobian; no isolated equilibrium for these inputs.');
    end
    x = x - step;
  end
  if nx > 0 && norm(dx) >= tol
    warning('Nelson:nflow_engine:trimNotConverged', ...
      sprintf('trim: did not converge (||xdot|| = %g after %d iterations).', ...
      norm(dx), maxit));
  end
end
%=============================================================================
function r = trim_linearize(documentJson, x, u)
  % Empty x / u leave the corresponding operating point at its default (post-INIT
  % state, zero inputs); the builtin skips empty arguments.
  resultJson = __nflow_linearize__(documentJson, x, u);
  r = jsondecode(resultJson);
  if ~isfield(r, 'status') || ~strcmp(r.status, 'done')
    reason = 'unknown error';
    if isfield(r, 'reason')
      reason = r.reason;
    end
    error('Nelson:nflow_engine:trim', 'trim failed: %s', reason);
  end
end
%=============================================================================
function documentJson = trim_model_json(model)
  if isnumeric(model) && isscalar(model)
    documentJson = trim_snapshot(model);
    return
  end
  if ~(ischar(model) || (isstring(model) && isscalar(model)))
    error('Nelson:nflow_engine:trimArgs', 'model must be a name, a handle or a .nflow file path.');
  end
  name = char(model);
  if bdIsLoaded(name)
    documentJson = trim_snapshot(name);
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
function documentJson = trim_snapshot(model)
  target = [tempname(), '.nflow'];
  cleanup = onCleanup(@() trim_delete_snapshot(target));
  save_system(model, target);
  documentJson = fileread(target);
end
%=============================================================================
function trim_delete_snapshot(target)
  if isfile(target)
    delete(target);
  end
end
%=============================================================================
