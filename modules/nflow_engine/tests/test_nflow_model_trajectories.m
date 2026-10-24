%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ENGLISH IMPOSED-->
%=============================================================================
% Each model below was authored in an independent block-diagram tool, simulated
% there to produce a committed expected trajectory (<name>.csv, columns
% time,value), and ported to a Nelson nflow model (<name>.nflow). We simulate
% the port with sim() and check that it reproduces the expected trajectory on
% that time grid. Every model exposes its plotted signal through a To Workspace
% block named <name>_out (Array format).
%
% Compare modes:
%   'max'  : max |nflow - expected| < tol (linear interp onto the grid).
%   'frac' : fraction of samples within tol >= 0.95 (nearest interp); for
%            switched signals whose only mismatch is the one-sample transition
%            convention at a discontinuity.
%
% Each mode also asserts the time grid on its own: tout is non-decreasing and
% the simulated window covers the expected span at both ends.
%=============================================================================
models_dir = modulepath('nflow_engine', 'tests');
%=============================================================================
% name | To Workspace variable | tolerance | mode
manifest = {...
  'abs_rectifier', 'abs_rectifier_out', 0.002, 'max'; ...
  'chirp_lowpass', 'chirp_lowpass_out', 0.0001, 'max'; ...
  'deadzone_sine', 'deadzone_sine_out', 0.002, 'max'; ...
  'derivative_of_sine', 'derivative_of_sine_out', 0.002, 'max'; ...
  'double_integrator', 'double_integrator_out', 1e-06, 'max'; ...
  'gain_bias_ramp', 'gain_bias_ramp_out', 1e-06, 'max'; ...
  'highpass_washout', 'highpass_washout_out', 1e-06, 'max'; ...
  'lookup_parabola_sine', 'lookup_parabola_sine_out', 0.005, 'max'; ...
  'mass_spring_damper', 'mass_spring_damper_out', 0.0001, 'max'; ...
  'overdamped_plant', 'overdamped_plant_out', 1e-06, 'max'; ...
  'pi_closed_loop', 'pi_closed_loop_out', 0.0001, 'max'; ...
  'product_two_sines', 'product_two_sines_out', 0.002, 'max'; ...
  'pulse_integrator', 'pulse_integrator_out', 0.001, 'max'; ...
  'pulse_rc_filter', 'pulse_rc_filter_out', 0.0001, 'max'; ...
  'rc_lowpass', 'rc_lowpass_out', 1e-06, 'max'; ...
  'resonant_second_order', 'resonant_second_order_out', 0.001, 'max'; ...
  'saturated_gain_sine', 'saturated_gain_sine_out', 0.005, 'max'; ...
  'saturating_integrator', 'saturating_integrator_out', 1e-06, 'max'; ...
  'sign_square', 'sign_square_out', 1e-06, 'frac'; ...
  'sine_plus_offset', 'sine_plus_offset_out', 0.002, 'max'; ...
  'sqrt_ramp', 'sqrt_ramp_out', 1e-06, 'max'; ...
  'state_space_oscillator', 'state_space_oscillator_out', 0.0001, 'max'; ...
  'trig_cos_ramp', 'trig_cos_ramp_out', 1e-06, 'max' ...
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
  % are allowed (a discontinuity records a before/after pair at the same t).
  asserts.istrue(all(diff(tn) >= 0));

  E = csvread(csvfile);
  te = E(:, 1);
  ye = E(:, 2);
  asserts.istrue(numel(te) > 10);
  % Time span, asserted before (and separately from) the values.
  dte = median(diff(te));
  asserts.istrue(tn(1) <= te(1) + 0.5 * dte);
  asserts.istrue(tn(end) >= te(end) - 0.5 * dte);

  switch mode
    case 'max'
      yi = interp1(tn, yn, te, 'linear');
      ok = ~isnan(yi);
      asserts.istrue(all(ok));
      maxAbsDiff = max(abs(yi(ok) - ye(ok)));
      asserts.istrue(maxAbsDiff < tol);
    case 'frac'
      yi = interp1(tn, yn, te, 'nearest');
      ok = ~isnan(yi);
      asserts.istrue(all(ok));
      frac = sum(abs(yi(ok) - ye(ok)) < tol) / sum(ok);
      asserts.istrue(frac >= 0.95);
    otherwise
      error(['unknown compare mode: ', mode]);
  end
end
%=============================================================================
