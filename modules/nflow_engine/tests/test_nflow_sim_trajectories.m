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
% <--ENGLISH IMPOSED-->
%=============================================================================
% Simulation trajectory tests: each nflow model has a committed expected
% trajectory (<name>.csv, columns time,value) produced by an independent
% simulation. We simulate the model with sim() and compare on that time grid.
%
% Compare modes:
%   'max'  : max |nflow - expected| under tol (linear interp onto the grid).
%   'frac' : fraction of samples matching within tol >= 0.95 (nearest interp);
%            for switched signals whose only mismatch is the one-sample
%            transition convention at a discontinuity.
%   'run'  : smoke only, no independent trajectory. Assert the model simulates
%            and yields at least <tol> finite samples (the tol column is reused
%            as a minimum sample count). For models whose plotted signal has no
%            single-trajectory expectation: keep them as a "does it run" net
%            instead of dropping them.
%
% Every mode also asserts the time grid on its own, independently of the values:
% tout is non-decreasing (no stall/backward step) and, for the trajectory modes,
% the simulated window covers the expected span at both ends.
%=============================================================================
models_dir = modulepath('nflow_engine', 'tests');
%=============================================================================
% name | To Workspace variable | tolerance (or min sample count for 'run') | mode | note
manifest = {...
  'Command', 'command_out', 1e-3, 'max'; ... % damped feedback, ~8e-5
  'Command_bode', 'command_bode_out', 5e-3, 'max'; ... % 4th-order transient, ~1.4e-3
  'Threshold_ZeroCrossing', 'threshold_out', 5e-3, 'max'; ... % IC oscillator drift, ~1.3e-3
  'if_then_else', 'if_then_else_out', 1e-6, 'max'; ... % pure sine, exact
  'Simple_Demo_square', 'square_out', 1e-6, 'frac'; ... % +/-1 square, exact but at transitions; recorded before the polarity fix, see below
  'Table_Lookup', 'table_lookup_out', 1e-6, 'frac'; ... % sawtooth->lookup1D, exact off sawtooth wraps
  'Multi_Frequency', 'multi_frequency_out', 100, 'run' ... % 3-input scope, no single trajectory; min 100 samples
};
%=============================================================================
for k = 1:size(manifest, 1)
  name = manifest{k, 1};
  varName = manifest{k, 2};
  tol = manifest{k, 3};
  mode = manifest{k, 4};

  model = [models_dir, '/', name, '.nflow'];
  csvfile = [models_dir, '/', name, '.csv'];

  out = sim(model);
  asserts.istrue(isfield(out, varName));
  asserts.istrue(isfield(out, 'tout'));

  tn = out.tout(:);
  yn = out.(varName)(:);
  asserts.isequal(numel(tn), numel(yn));
  asserts.istrue(numel(tn) > 1);
  % Time grid, checked on its own: tout never runs backwards. Equal timestamps
  % are allowed (a discontinuity records a before/after pair at the same t), a
  % decreasing one is always a solver bug.
  asserts.istrue(all(diff(tn) >= 0));

  switch mode
    case 'run'
      % No trajectory to compare against: assert it produced enough finite samples.
      asserts.istrue(numel(yn) >= tol);
      asserts.istrue(all(isfinite(yn)));
    case {'max', 'frac'}
      E = csvread(csvfile);
      te = E(:, 1);
      ye = E(:, 2);
      if strcmp(name, 'Simple_Demo_square')
        % The recorded trajectory predates the signal generator's polarity
        % being put back the right way round (a square wave opens its period on
        % the LOW half). The numbers are still the independent record they
        % always were; only the sign convention of the block under them moved,
        % so they are read with the sign it has now.
        ye = -ye;
      end
      asserts.istrue(numel(te) > 10);
      % Time span, asserted before (and separately from) the values: the
      % simulated window must reach both ends of the expected grid.
      dte = median(diff(te));
      asserts.istrue(tn(1) <= te(1) + 0.5 * dte); % starts at the origin
      asserts.istrue(tn(end) >= te(end) - 0.5 * dte); % no early stop
      switch mode
        case 'max'
          yi = interp1(tn, yn, te, 'linear');
          ok = ~isnan(yi);
          asserts.istrue(all(ok)); % every expected sample inside the simulated span
          maxAbsDiff = max(abs(yi(ok) - ye(ok)));
          asserts.istrue(maxAbsDiff < tol);
        case 'frac'
          yi = interp1(tn, yn, te, 'nearest');
          ok = ~isnan(yi);
          asserts.istrue(all(ok));
          frac = sum(abs(yi(ok) - ye(ok)) < tol) / sum(ok);
          asserts.istrue(frac >= 0.95);
      end
    otherwise
      error(['unknown compare mode: ', mode]);
  end
end
%=============================================================================
