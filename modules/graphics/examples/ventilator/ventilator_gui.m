%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
%=============================================================================
% Bi-level (PC-AC) ventilator monitor - 100% Nelson (uifigure + drawnow loop)
% Single-compartment lung: dV/dt = (Pdrive + Pmus - V/C) / R
% Three control modes:
%   * Ideal pressure  - airway pressure imposed directly (no controller)
%   * Pressure servo  - PID drives the delivered pressure to the bilevel setpoint
%   * Volume target   - per-breath PID adapts the driving pressure to a target Vt
% Responsive layout. Educational model - not for clinical use.
%=============================================================================
function ventilator_gui()
  global VENT
  VENT = struct();

  % ---- model parameters ----
  VENT.PEEP = 5; VENT.PI = 18; VENT.RR = 15; VENT.Ti = 1.2;
  VENT.R = 10; VENT.C = 50; VENT.Pm = 3; VENT.spont = true;
  VENT.mode = 1; % 1 ideal, 2 servo, 3 prvc
  VENT.Kp = 12; VENT.Ki = 0; VENT.Kd = 0; VENT.Vtgt = 500;
  VENT.Rise = 0.15; % pressure rise time / slope (s)
  VENT.Trig = 3; % flow trigger sensitivity (L/min)
  VENT.RRsp = 18; % patient spontaneous rate (/min)
  VENT.almPmax = 35; VENT.almVtLo = 300; % alarm limits (cmH2O, mL)
  VENT.Leak = 0; % circuit/mask leak (L/min per cmH2O)
  VENT.running = true; VENT.alive = true;

  % ---- simulation / controller state ----
  VENT.V = 0; VENT.t = 0; VENT.WIN = 10; VENT.h = 0.005;
  VENT.bt = []; VENT.bp = []; VENT.bf = []; VENT.bv = [];
  VENT.lt = []; VENT.lp = []; VENT.lv = [];
  VENT = ctrl_reset(VENT);

  ink = [0.90 0.94 0.97]; bg = [0.039 0.059 0.078]; axbg = [0.055 0.090 0.120];
  axln = [0.42 0.54 0.64];
  cPress = [0.96 0.79 0.09]; cFlow = [0.21 0.82 0.50];
  cVol = [0.30 0.80 0.94]; cLoop = [0.97 0.15 0.52];

  fig = uifigure('Name', 'NelVent - Bi-level (PC-AC) ventilator monitor', ...
    'Position', [40 40 1180 660], 'AutoResizeChildren', 'off', 'Tag', 'ventGuiFigure', ...
    'Visible', 'off'); % stay hidden until the first layout is done (no flash of unplaced widgets)
  fig.Color = bg;
  VENT.fig = fig;

  VENT.axP = mkaxes(fig, 'Pressure (cmH2O)', axbg, axln, ink);
  VENT.axF = mkaxes(fig, 'Flow (L/min)', axbg, axln, ink);
  VENT.axV = mkaxes(fig, 'Volume (mL)', axbg, axln, ink);
  VENT.axL = mkaxes(fig, 'Pressure-Volume loop', axbg, axln, ink);
  xlabel(VENT.axL, 'P (cmH2O)'); ylabel(VENT.axL, 'V (mL)');

  VENT.linP = plot(VENT.axP, NaN, NaN, 'Color', cPress, 'LineWidth', 1.8);
  VENT.linF = plot(VENT.axF, NaN, NaN, 'Color', cFlow, 'LineWidth', 1.8);
  VENT.linV = plot(VENT.axV, NaN, NaN, 'Color', cVol, 'LineWidth', 1.8);
  VENT.linL = plot(VENT.axL, NaN, NaN, 'Color', cLoop, 'LineWidth', 1.8);
  VENT.linMkI = plot(VENT.axP, NaN, NaN, 'Color', [0.21 0.82 0.50], 'LineWidth', 1.2); % insp trigger
  VENT.linMkE = plot(VENT.axP, NaN, NaN, 'Color', [1.00 0.66 0.30], 'LineWidth', 1.2); % exp cycling
  VENT.legI = uilabel(fig, 'Text', '| insp trigger', 'FontColor', [0.21 0.82 0.50], 'FontSize', 10);
  VENT.legE = uilabel(fig, 'Text', '| exp cycling', 'FontColor', [1.00 0.66 0.30], 'FontSize', 10);

  VENT.monTitle = uilabel(fig, 'Text', 'MONITORING', 'FontColor', axln, 'FontSize', 12);
  [VENT.mVtiN, VENT.mVti] = mkmon(fig, 'VTi inspired', cVol, 28);
  [VENT.mVteN, VENT.mVte] = mkmon(fig, 'VTe expired', cVol, 28);
  [VENT.mMVN, VENT.mMV] = mkmon(fig, 'MV minute', ink, 22);
  [VENT.mPpkN, VENT.mPpk] = mkmon(fig, 'P peak', ink, 22);
  [VENT.mPmnN, VENT.mPmn] = mkmon(fig, 'P mean', ink, 22);
  [VENT.mIEN, VENT.mIE] = mkmon(fig, 'I : E', ink, 22);
  [VENT.mPltN, VENT.mPlt] = mkmon(fig, 'P plat', ink, 22);
  [VENT.mPEtN, VENT.mPEt] = mkmon(fig, 'PEEP total', ink, 22);

  VENT.modeLbl = uilabel(fig, 'Text', 'Control mode', 'FontColor', ink, 'FontSize', 12);
  VENT.modeDrop = uidropdown(fig, 'Items', ...
    {'Ideal pressure', 'Pressure servo (PID)', 'Volume target (PRVC)'}, ...
    'Value', 'Ideal pressure', 'ValueChangedFcn', @vt_mode);
  VENT.presetLbl = uilabel(fig, 'Text', 'Preset', 'FontColor', ink, 'FontSize', 12);
  VENT.presetDrop = uidropdown(fig, 'Items', ...
    {'Preset...', 'Normal adult', 'ARDS', 'COPD', 'Air trapping', 'Pediatric'}, ...
    'Value', 'Preset...', 'ValueChangedFcn', @vt_preset);

  VENT.bFreeze = uibutton(fig, 'Text', 'Freeze', 'ButtonPushedFcn', @vt_freeze);
  VENT.bReset = uibutton(fig, 'Text', 'Reset', 'ButtonPushedFcn', @vt_reset);
  VENT.bManual = uibutton(fig, 'Text', 'Manual breath', 'ButtonPushedFcn', @vt_manual);
  VENT.bHold = uibutton(fig, 'Text', 'Insp. hold', 'ButtonPushedFcn', @vt_hold);

  [VENT.lPEEP, VENT.sPEEP] = mkslider(fig, VENT.PEEP, [0 15], ink, @vt_peep, 'PEEP / EPAP', 'cmH2O');
  [VENT.lPI, VENT.sPI] = mkslider(fig, VENT.PI, [8 35], ink, @vt_pi, 'Pinsp / IPAP', 'cmH2O');
  [VENT.lRR, VENT.sRR] = mkslider(fig, VENT.RR, [6 35], ink, @vt_rr, 'Resp. rate', '/min');
  [VENT.lTi, VENT.sTi] = mkslider(fig, VENT.Ti, [0.4 2.6], ink, @vt_ti, 'Insp. time Ti', 's');
  [VENT.lR, VENT.sR] = mkslider(fig, VENT.R, [3 30], ink, @vt_r, 'Airway resistance R', 'cmH2O.s/L');
  [VENT.lC, VENT.sC] = mkslider(fig, VENT.C, [15 90], ink, @vt_c, 'Compliance C', 'mL/cmH2O');
  [VENT.lPm, VENT.sPm] = mkslider(fig, VENT.Pm, [0 8], ink, @vt_pm, 'Patient effort Pmus', 'cmH2O');
  VENT.cbSp = uicheckbox(fig, 'Text', 'Assisted breaths', 'Value', true, ...
    'FontColor', ink, 'ValueChangedFcn', @vt_spont);
  [VENT.lKp, VENT.sKp] = mkslider(fig, VENT.Kp, [0 30], ink, @vt_kp, 'PID Kp', '');
  [VENT.lKi, VENT.sKi] = mkslider(fig, VENT.Ki, [0 20], ink, @vt_ki, 'PID Ki', '');
  [VENT.lKd, VENT.sKd] = mkslider(fig, VENT.Kd, [0 5], ink, @vt_kd, 'PID Kd', '');
  [VENT.lRise, VENT.sRise] = mkslider(fig, VENT.Rise, [0 1.0], ink, @vt_rise, 'Rise time', 's');
  [VENT.lVt, VENT.sVt] = mkslider(fig, VENT.Vtgt, [200 800], ink, @vt_vtgt, 'Target Vt (PRVC)', 'mL');
  [VENT.lTrig, VENT.sTrig] = mkslider(fig, VENT.Trig, [0.5 15], ink, @vt_trig, 'Trigger (flow)', 'L/min');
  [VENT.lRRsp, VENT.sRRsp] = mkslider(fig, VENT.RRsp, [0 30], ink, @vt_rrsp, 'Patient rate', '/min');
  [VENT.lPmax, VENT.sPmax] = mkslider(fig, VENT.almPmax, [20 50], ink, @vt_pmax, 'Alarm Pmax', 'cmH2O');
  [VENT.lVlo, VENT.sVlo] = mkslider(fig, VENT.almVtLo, [100 500], ink, @vt_vlo, 'Alarm Vt low', 'mL');
  [VENT.lLeak, VENT.sLeak] = mkslider(fig, VENT.Leak, [0 1.5], ink, @vt_leak, 'Leak', 'L/min/cmH2O');
  VENT.almBanner = uilabel(fig, 'Text', '', 'FontColor', [1 0.33 0.44], 'FontSize', 13);

  vt_layout();
  fig.SizeChangedFcn = @vt_resize;
  fig.CloseRequestFcn = @vt_close;
  % fit the window to the visible screen area so the bottom controls are on-screen
  try
    ss = get(groot, 'ScreenSize');
    w = min(1280, ss(3) - 80); hgt = min(820, ss(4) - 120);
    fig.Position = [ss(1) + 30, ss(2) + 50, max(900, w), max(600, hgt)];
  catch
  end
  drawnow();
  vt_layout();

  vt_prime();
  vt_refresh();
  fig.Visible = 'on'; % reveal once every widget sits at its final place
  drawnow();
  vt_loop();
end
%=============================================================================
function S = ctrl_reset(S)
  S.Pdel = 0; % servo: delivered driving pressure (above PEEP)
  S.eI = 0; S.ePrev = 0; % servo PID state
  S.dPauto = S.PI - S.PEEP; % prvc: adapting driving pressure
  S.vI = 0; S.vPrev = 0; % prvc PID state
  S.cycMin = 0; S.cycMax = 0; S.prevPh = 0; % per-breath Vt tracking
  S.bphase = 0; % breath clock (s since inspiration start)
  S.vStart = 0; S.vPeak = 0; % breath volume markers (mL)
  S.VTi = 0; S.VTe = 0; S.Vtrap = 0; % inspired / expired / trapped (mL)
  S.manTrig = false; % manual-breath trigger request
  S.holdT = 0; % inspiratory-hold countdown (s)
  S.leakInsp = 0; % volume lost to leak during inspiration (mL)
  S.Pplat = 0; % measured plateau pressure (cmH2O)
  S.evI = []; S.evE = []; S.prevInsp = false; % inspiratory / expiratory trigger times
end
%=============================================================================
function vt_resize(~, ~), vt_layout(); end
%=============================================================================
function vt_layout()
  global VENT
  p = VENT.fig.Position;
  W = max(760, p(3)); H = max(560, p(4));
  m = 18; gap = 18; gv = 12;

  colW = max(150, floor((W - 2 * m - 3 * gap) / 4));
  xc = @(i) m + (i - 1) * (colW + gap);
  sh = 16; % slider height (must be tall enough to paint)
  rowV = 246; rowP = 178; rowC = 110; rowD = 42; % four control rows (slider y)
  place(VENT.sPEEP, [xc(1) rowV colW sh]); place(VENT.lPEEP, [xc(1) rowV + sh + 4 colW + 40 18]);
  place(VENT.sPI, [xc(2) rowV colW sh]); place(VENT.lPI, [xc(2) rowV + sh + 4 colW + 40 18]);
  place(VENT.sRR, [xc(3) rowV colW sh]); place(VENT.lRR, [xc(3) rowV + sh + 4 colW + 40 18]);
  place(VENT.sTi, [xc(4) rowV colW sh]); place(VENT.lTi, [xc(4) rowV + sh + 4 colW + 40 18]);
  place(VENT.sR, [xc(1) rowP colW sh]); place(VENT.lR, [xc(1) rowP + sh + 4 colW + 40 18]);
  place(VENT.sC, [xc(2) rowP colW sh]); place(VENT.lC, [xc(2) rowP + sh + 4 colW + 40 18]);
  place(VENT.sPm, [xc(3) rowP colW sh]); place(VENT.lPm, [xc(3) rowP + sh + 4 colW + 40 18]);
  place(VENT.cbSp, [xc(4) rowP + 2 220 24]);
  place(VENT.sKp, [xc(1) rowC colW sh]); place(VENT.lKp, [xc(1) rowC + sh + 4 colW + 40 18]);
  place(VENT.sKi, [xc(2) rowC colW sh]); place(VENT.lKi, [xc(2) rowC + sh + 4 colW + 40 18]);
  place(VENT.sKd, [xc(3) rowC colW sh]); place(VENT.lKd, [xc(3) rowC + sh + 4 colW + 40 18]);
  place(VENT.sRise, [xc(4) rowC colW sh]); place(VENT.lRise, [xc(4) rowC + sh + 4 colW + 40 18]);
  place(VENT.sTrig, [xc(1) rowD colW sh]); place(VENT.lTrig, [xc(1) rowD + sh + 4 colW + 40 18]);
  place(VENT.sRRsp, [xc(2) rowD colW sh]); place(VENT.lRRsp, [xc(2) rowD + sh + 4 colW + 40 18]);
  place(VENT.sPmax, [xc(3) rowD colW sh]); place(VENT.lPmax, [xc(3) rowD + sh + 4 colW + 40 18]);
  place(VENT.sVlo, [xc(4) rowD colW sh]); place(VENT.lVlo, [xc(4) rowD + sh + 4 colW + 40 18]);

  controlsH = 296;
  topY = controlsH + m;
  topH = max(220, H - topY - m);

  rightW = min(480, max(330, round(W * 0.34)));
  leftW = max(320, W - rightW - 3 * m);
  xL = m; xR = m + leftW + m;
  rightW = W - xR - m;

  waveH = floor((topH - 2 * gv) / 3);
  yV = topY; yF = yV + waveH + gv; yP = yF + waveH + gv;
  place(VENT.axP, [xL yP leftW waveH]);
  place(VENT.axF, [xL yF leftW waveH]);
  place(VENT.axV, [xL yV leftW waveH]);
  place(VENT.legI, [xL + leftW - 210, yP + waveH - 16, 100, 14]);
  place(VENT.legE, [xL + leftW - 105, yP + waveH - 16, 100, 14]);

  loopH = min(round(topH * 0.26), rightW);
  yLoop = topY + topH - loopH;
  place(VENT.axL, [xR yLoop rightW loopH]);

  half = floor((rightW - 10) / 2);
  xR2 = xR + half + 10;
  yy = yLoop - 22;
  place(VENT.monTitle, [xR yy rightW 20]);
  rA = yy - 46; rB = rA - 36; rC = rB - 36; rD = rC - 36;
  moncell(VENT.mVtiN, VENT.mVti, [xR rA half 40]);
  moncell(VENT.mVteN, VENT.mVte, [xR2 rA half 40]);
  moncell(VENT.mMVN, VENT.mMV, [xR rB half 30]);
  moncell(VENT.mPpkN, VENT.mPpk, [xR2 rB half 30]);
  moncell(VENT.mPmnN, VENT.mPmn, [xR rC half 30]);
  moncell(VENT.mIEN, VENT.mIE, [xR2 rC half 30]);
  moncell(VENT.mPltN, VENT.mPlt, [xR rD half 30]);
  moncell(VENT.mPEtN, VENT.mPEt, [xR2 rD half 30]);

  % control cluster, anchored to the bottom of the right column
  bY = topY + 2; % four action buttons on one row
  bw = floor((rightW - 3 * 8) / 4);
  place(VENT.bFreeze, [xR bY bw 30]);
  place(VENT.bReset, [xR + (bw + 8) bY bw 30]);
  place(VENT.bManual, [xR + 2 * (bw + 8) bY bw 30]);
  place(VENT.bHold, [xR + 3 * (bw + 8) bY bw 30]);
  vy = bY + 40; % Target Vt (left) + Leak (right)
  place(VENT.lVt, [xR vy + sh + 2 half 16]); place(VENT.sVt, [xR vy half sh]);
  place(VENT.lLeak, [xR2 vy + sh + 2 half 16]); place(VENT.sLeak, [xR2 vy half sh]);
  dy = vy + 46;
  place(VENT.presetLbl, [xR dy + 26 half 16]); place(VENT.presetDrop, [xR dy half 24]);
  place(VENT.modeLbl, [xR2 dy + 26 half 16]); place(VENT.modeDrop, [xR2 dy half 24]);
  place(VENT.almBanner, [xR dy + 48 rightW 18]);
end
%=============================================================================
function place(h, pos)
  if pos(3) < 1, pos(3) = 1; end
  if pos(4) < 1, pos(4) = 1; end
  if is_uislider(h)
    % a uislider keeps a fixed 3 px thickness on its minor axis; honour it so the
    % generic layout never fights the component (no "cannot be changed" warning).
    if strcmp(h.Orientation, 'vertical'), pos(3) = 3; else, pos(4) = 3; end
  end
  h.Position = pos;
end
%=============================================================================
function tf = is_uislider(h)
  tf = false;
  try, tf = any(strcmp(h.Type, {'uislider', 'uirangeslider'})); catch, end
end
%=============================================================================
function moncell(nameLbl, valLbl, rect)
  place(nameLbl, [rect(1) rect(2) + rect(4) - 15 rect(3) 14]);
  place(valLbl, [rect(1) rect(2) - 2 rect(3) rect(4) - 14]);
end
%=============================================================================
function [xs, ys] = vt_vsegs(times, ymax)
  % vertical marker segments (NaN-separated) for the trigger event lines
  xs = []; ys = [];
  for i = 1:numel(times)
    xs = [xs, times(i), times(i), NaN];
    ys = [ys, 0, ymax, NaN];
  end
end
%=============================================================================
function ax = mkaxes(parent, ttl, axbg, axln, ink)
  ax = uiaxes(parent, 'Position', [10 10 200 120]);
  set(ax, 'Color', axbg, 'XColor', axln, 'YColor', axln, 'GridColor', [0.20 0.30 0.40]);
  grid(ax, 'on'); hold(ax, 'on');
  title(ax, ttl, 'Color', ink, 'FontWeight', 'normal');
end
%=============================================================================
function [nameLbl, valLbl] = mkmon(parent, name, valcol, fs)
  nameLbl = uilabel(parent, 'Text', name, 'Position', [10 10 100 14], ...
    'FontColor', [0.42 0.54 0.64], 'FontSize', 10);
  valLbl = uilabel(parent, 'Text', '--', 'Position', [10 10 100 30], ...
    'FontColor', valcol, 'FontSize', fs);
end
%=============================================================================
function [lbl, s] = mkslider(parent, val, lims, ink, cb, name, unit)
  lbl = uilabel(parent, 'Text', labtxt(name, val, unit), ...
    'Position', [10 10 200 20], 'FontColor', ink, 'FontSize', 12);
  s = uislider(parent, 'Limits', lims, 'Value', val, 'Position', [10 10 200 3], ...
    'ValueChangedFcn', cb);
  s.UserData = struct('label', lbl, 'name', name, 'unit', unit);
end
%=============================================================================
function txt = labtxt(name, val, unit)
  if isempty(unit), txt = sprintf('%s:  %g', name, val);
  else, txt = sprintf('%s:  %g %s', name, val, unit); end
end
%=============================================================================
function setlab(src)
  u = src.UserData;
  u.label.Text = labtxt(u.name, src.Value, u.unit);
end
%=============================================================================
function vt_peep(src, ~), global VENT; VENT.PEEP = src.Value; setlab(src); end
function vt_pi(src, ~), global VENT; VENT.PI = src.Value; setlab(src); end
function vt_rr(src, ~), global VENT; VENT.RR = src.Value; setlab(src); end
function vt_ti(src, ~), global VENT; VENT.Ti = src.Value; setlab(src); end
function vt_r(src, ~), global VENT; VENT.R = src.Value; setlab(src); end
function vt_c(src, ~), global VENT; VENT.C = src.Value; setlab(src); end
function vt_pm(src, ~), global VENT; VENT.Pm = src.Value; setlab(src); end
function vt_kp(src, ~), global VENT; VENT.Kp = src.Value; setlab(src); end
function vt_ki(src, ~), global VENT; VENT.Ki = src.Value; setlab(src); end
function vt_kd(src, ~), global VENT; VENT.Kd = src.Value; setlab(src); end
function vt_vtgt(src, ~), global VENT; VENT.Vtgt = src.Value; setlab(src); end
function vt_rise(src, ~), global VENT; VENT.Rise = src.Value; setlab(src); end
function vt_trig(src, ~), global VENT; VENT.Trig = src.Value; setlab(src); end
function vt_rrsp(src, ~), global VENT; VENT.RRsp = src.Value; setlab(src); end
function vt_pmax(src, ~), global VENT; VENT.almPmax = src.Value; setlab(src); end
function vt_vlo(src, ~), global VENT; VENT.almVtLo = src.Value; setlab(src); end
function vt_leak(src, ~), global VENT; VENT.Leak = src.Value; setlab(src); end
function vt_spont(src, ~), global VENT; VENT.spont = logical(src.Value); end
%=============================================================================
function vt_hold(~, ~)
  global VENT
  VENT.holdT = 2.0; % end-inspiratory hold (s): freezes flow to reveal Pplat
end
%=============================================================================
function vt_preset(src, ~)
  global VENT
  switch src.Value
    case 'Normal adult'
      VENT.PEEP = 5; VENT.PI = 18; VENT.RR = 15; VENT.Ti = 1.2; VENT.R = 10; VENT.C = 50; VENT.Pm = 3; VENT.Vtgt = 500;
    case 'ARDS'
      VENT.PEEP = 12; VENT.PI = 28; VENT.RR = 24; VENT.Ti = 0.8; VENT.R = 12; VENT.C = 30; VENT.Pm = 2; VENT.Vtgt = 400;
    case 'COPD'
      VENT.PEEP = 5; VENT.PI = 20; VENT.RR = 12; VENT.Ti = 1.0; VENT.R = 25; VENT.C = 60; VENT.Pm = 4; VENT.Vtgt = 550;
    case 'Air trapping'
      VENT.PEEP = 5; VENT.PI = 20; VENT.RR = 28; VENT.Ti = 1.3; VENT.R = 20; VENT.C = 55; VENT.Pm = 3; VENT.Vtgt = 500;
    case 'Pediatric'
      VENT.PEEP = 5; VENT.PI = 18; VENT.RR = 25; VENT.Ti = 0.6; VENT.R = 20; VENT.C = 15; VENT.Pm = 1; VENT.Vtgt = 180;
    otherwise
      return
  end
  vt_sync_sliders();
end
%=============================================================================
function vt_sync_sliders()
  global VENT
  VENT.sPEEP.Value = VENT.PEEP; VENT.sPI.Value = VENT.PI; VENT.sRR.Value = VENT.RR; VENT.sTi.Value = VENT.Ti;
  VENT.sR.Value = VENT.R; VENT.sC.Value = VENT.C; VENT.sPm.Value = VENT.Pm; VENT.sVt.Value = VENT.Vtgt;
  setlab(VENT.sPEEP); setlab(VENT.sPI); setlab(VENT.sRR); setlab(VENT.sTi);
  setlab(VENT.sR); setlab(VENT.sC); setlab(VENT.sPm); setlab(VENT.sVt);
  VENT.V = 0; VENT.t = 0;
  VENT.bt = []; VENT.bp = []; VENT.bf = []; VENT.bv = []; VENT.lt = []; VENT.lp = []; VENT.lv = [];
  VENT = ctrl_reset(VENT);
  vt_prime(); vt_refresh();
end
%=============================================================================
function vt_mode(src, ~)
  global VENT
  switch src.Value
    case 'Pressure servo (PID)', VENT.mode = 2;
    case 'Volume target (PRVC)', VENT.mode = 3;
    otherwise, VENT.mode = 1;
  end
  VENT = ctrl_reset(VENT);
end
%=============================================================================
function vt_freeze(src, ~)
  global VENT
  VENT.running = ~VENT.running;
  if VENT.running, src.Text = 'Freeze'; else, src.Text = 'Resume'; end
end
%=============================================================================
function vt_manual(~, ~)
  global VENT
  VENT.manTrig = true; % request one extra (assisted) breath now
end
%=============================================================================
function vt_reset(~, ~)
  global VENT
  VENT.PEEP = 5; VENT.PI = 18; VENT.RR = 15; VENT.Ti = 1.2;
  VENT.R = 10; VENT.C = 50; VENT.Pm = 3; VENT.spont = true;
  VENT.Kp = 12; VENT.Ki = 0; VENT.Kd = 0; VENT.Vtgt = 500; VENT.Rise = 0.15;
  VENT.Trig = 3; VENT.RRsp = 18; VENT.almPmax = 35; VENT.almVtLo = 300; VENT.Leak = 0; VENT.running = true;
  VENT.V = 0; VENT.t = 0;
  VENT.bt = []; VENT.bp = []; VENT.bf = []; VENT.bv = [];
  VENT.lt = []; VENT.lp = []; VENT.lv = [];
  VENT = ctrl_reset(VENT);
  VENT.sPEEP.Value = 5; VENT.sPI.Value = 18; VENT.sRR.Value = 15; VENT.sTi.Value = 1.2;
  VENT.sR.Value = 10; VENT.sC.Value = 50; VENT.sPm.Value = 3; VENT.cbSp.Value = true;
  VENT.sKp.Value = 12; VENT.sKi.Value = 0; VENT.sKd.Value = 0; VENT.sVt.Value = 500; VENT.sRise.Value = 0.15;
  VENT.sTrig.Value = 3; VENT.sRRsp.Value = 18; VENT.sPmax.Value = 35; VENT.sVlo.Value = 300; VENT.sLeak.Value = 0;
  setlab(VENT.sPEEP); setlab(VENT.sPI); setlab(VENT.sRR); setlab(VENT.sTi);
  setlab(VENT.sR); setlab(VENT.sC); setlab(VENT.sPm);
  setlab(VENT.sKp); setlab(VENT.sKi); setlab(VENT.sKd); setlab(VENT.sVt); setlab(VENT.sRise);
  setlab(VENT.sTrig); setlab(VENT.sRRsp); setlab(VENT.sPmax); setlab(VENT.sVlo); setlab(VENT.sLeak);
  VENT.bFreeze.Text = 'Freeze';
  vt_prime(); vt_refresh();
end
%=============================================================================
function vt_close(~, ~)
  global VENT
  VENT.alive = false;
  try, delete(VENT.fig); catch, end
end
%=============================================================================
function vt_step()
  global VENT
  T = 60 / VENT.RR; h = VENT.h; Cl = VENT.C / 1000;
  Vml0 = VENT.V * 1000; % lung volume at start of this step

  % --- inspiratory hold: freeze flow so airway pressure equilibrates to Pplat ---
  if VENT.holdT > 0
    VENT.holdT = VENT.holdT - h;
    VENT.t = VENT.t + h;
    Palv = VENT.PEEP + VENT.V / Cl; % no flow -> airway = alveolar (plateau)
    VENT.Pplat = Palv;
    Vml = VENT.V * 1000; if Vml > VENT.vPeak, VENT.vPeak = Vml; end
    VENT.bt(end + 1) = VENT.t; VENT.bp(end + 1) = Palv; VENT.bf(end + 1) = 0; VENT.bv(end + 1) = Vml;
    VENT.lt(end + 1) = VENT.t; VENT.lp(end + 1) = Palv; VENT.lv(end + 1) = Vml;
    return
  end

  % --- patient spontaneous effort on its own clock (may trigger the machine) ---
  mus = 0; spTrig = false;
  if VENT.spont
    Tsp = 60 / max(1, VENT.RRsp);
    spph = mod(VENT.t, Tsp);
    if spph < 0.8, mus = VENT.Pm * sin(pi * spph / 0.8); end
    if (mus / VENT.R) * 60 >= VENT.Trig, spTrig = true; end % inspiratory flow vs sensitivity
  end

  % --- breath scheduling: mandatory backup rate OR patient/manual trigger (assist) ---
  mandatory = VENT.bphase >= T;
  triggered = (spTrig && VENT.bphase >= VENT.Ti + 0.15) || VENT.manTrig;
  if mandatory || triggered
    VENT.VTi = max(0, VENT.vPeak - VENT.vStart) + VENT.leakInsp; % machine-inspired
    VENT.VTe = max(0, VENT.vPeak - Vml0); % expired tidal volume
    VENT.Vtrap = max(0, Vml0); % end-expiratory volume above FRC
    if VENT.mode == 3 % PRVC: adapt drive to hit Vtgt
      e = VENT.Vtgt - VENT.VTi;
      VENT.vI = VENT.vI + e;
      d = e - VENT.vPrev; VENT.vPrev = e;
      upd = 0.006 * VENT.Kp * e + 0.0015 * VENT.Ki * VENT.vI + 0.02 * VENT.Kd * d;
      VENT.dPauto = VENT.dPauto + upd;
      if VENT.dPauto < 2, VENT.dPauto = 2; end
      if VENT.dPauto > 45, VENT.dPauto = 45; end
    end
    VENT.vStart = Vml0; VENT.vPeak = Vml0; VENT.leakInsp = 0;
    VENT.bphase = 0; VENT.manTrig = false;
    VENT.evI(end + 1) = VENT.t; % inspiratory trigger marker
  else
    VENT.bphase = VENT.bphase + h;
  end

  bp = VENT.bphase;
  insp = bp < VENT.Ti;
  if VENT.prevInsp && ~insp, VENT.evE(end + 1) = VENT.t; end % expiratory (cycling) marker
  VENT.prevInsp = insp;
  ramp = 1; % pressure rise time / slope
  if insp && VENT.Rise > 0.001, ramp = min(1, bp / VENT.Rise); end

  if VENT.mode == 2
    sp = 0; if insp, sp = (VENT.PI - VENT.PEEP) * ramp; end
    e = sp - VENT.Pdel;
    VENT.eI = VENT.eI + e * h;
    der = (e - VENT.ePrev) / h; VENT.ePrev = e;
    u = VENT.Kp * e + VENT.Ki * VENT.eI + VENT.Kd * der;
    VENT.Pdel = VENT.Pdel + u * h;
    if VENT.Pdel < -VENT.PEEP, VENT.Pdel = -VENT.PEEP; end
    if VENT.Pdel > 60, VENT.Pdel = 60; end
    dr = VENT.Pdel;
  elseif VENT.mode == 3
    dr = 0; if insp, dr = VENT.dPauto * ramp; end
  else
    dr = 0; if insp, dr = (VENT.PI - VENT.PEEP) * ramp; end
  end

  Q = (dr + mus - VENT.V / Cl) / VENT.R;
  VENT.V = VENT.V + Q * h; if VENT.V < 0, VENT.V = 0; end
  VENT.t = VENT.t + h;
  Vml = VENT.V * 1000;
  if Vml > VENT.vPeak, VENT.vPeak = Vml; end
  Paw = VENT.PEEP + dr;
  if insp, VENT.leakInsp = VENT.leakInsp + (VENT.Leak / 60 * Paw) * 1000 * h; end % mL lost to leak
  VENT.bt(end + 1) = VENT.t; VENT.bp(end + 1) = Paw;
  VENT.bf(end + 1) = Q * 60; VENT.bv(end + 1) = Vml;
  VENT.lt(end + 1) = VENT.t; VENT.lp(end + 1) = Paw; VENT.lv(end + 1) = Vml;
end
%=============================================================================
function vt_trim()
  global VENT
  m = VENT.bt >= (VENT.t - VENT.WIN);
  VENT.bt = VENT.bt(m); VENT.bp = VENT.bp(m); VENT.bf = VENT.bf(m); VENT.bv = VENT.bv(m);
  mL = VENT.lt >= (VENT.t - 2.2 * (60 / VENT.RR));
  VENT.lt = VENT.lt(mL); VENT.lp = VENT.lp(mL); VENT.lv = VENT.lv(mL);
  tmin = VENT.t - VENT.WIN;
  if ~isempty(VENT.evI), VENT.evI = VENT.evI(VENT.evI >= tmin); end
  if ~isempty(VENT.evE), VENT.evE = VENT.evE(VENT.evE >= tmin); end
end
%=============================================================================
function vt_prime()
  global VENT
  for k = 1:round(9 / VENT.h), vt_step(); end
  vt_trim();
end
%=============================================================================
function vt_refresh()
  global VENT
  t1 = VENT.t; t0 = VENT.t - VENT.WIN;
  set(VENT.linP, 'XData', VENT.bt, 'YData', VENT.bp);
  set(VENT.linF, 'XData', VENT.bt, 'YData', VENT.bf);
  set(VENT.linV, 'XData', VENT.bt, 'YData', VENT.bv);
  set(VENT.linL, 'XData', VENT.lp, 'YData', VENT.lv);
  ymaxP = max(VENT.PI + 6, 22);
  xlim(VENT.axP, [t0 t1]); ylim(VENT.axP, [0 ymaxP]);
  [xi, yi] = vt_vsegs(VENT.evI, ymaxP); set(VENT.linMkI, 'XData', xi, 'YData', yi);
  [xe, ye] = vt_vsegs(VENT.evE, ymaxP); set(VENT.linMkE, 'XData', xe, 'YData', ye);
  xlim(VENT.axF, [t0 t1]); ylim(VENT.axF, [-120 140]);
  xlim(VENT.axV, [t0 t1]); ylim(VENT.axV, [0 max(200, (VENT.PI - VENT.PEEP) * VENT.C * 1.3)]);
  xlim(VENT.axL, [VENT.PEEP - 2, VENT.PI + 4]);
  ylim(VENT.axL, [0 max(200, (VENT.PI - VENT.PEEP) * VENT.C * 1.25)]);
  T = 60 / VENT.RR; m = VENT.bt >= (VENT.t - T);
  if any(m)
    Pp = VENT.bp(m);
    VENT.mPpk.Text = sprintf('%.1f', max(Pp));
    VENT.mPmn.Text = sprintf('%.1f', mean(Pp));
  end
  VENT.mVti.Text = sprintf('%d mL', round(VENT.VTi));
  VENT.mVte.Text = sprintf('%d mL', round(VENT.VTe));
  VENT.mMV.Text = sprintf('%.1f L/min', VENT.VTe / 1000 * VENT.RR);
  PEEPi = VENT.Vtrap / VENT.C; % auto-PEEP (cmH2O)
  Pplat = VENT.PEEP + VENT.VTi / VENT.C; % static end-insp pressure
  VENT.mPlt.Text = sprintf('%.1f', Pplat);
  VENT.mPEt.Text = sprintf('%.1f', VENT.PEEP + PEEPi);
  if VENT.VTi < 300 || VENT.VTi > 700, VENT.mVti.FontColor = [1 0.33 0.44];
  else, VENT.mVti.FontColor = [0.30 0.80 0.94]; end
  Te = T - VENT.Ti;
  VENT.mIE.Text = sprintf('1 : %.1f', Te / VENT.Ti);

  % ---- alarms (adjustable limits) ----
  pk = 0; if any(m), pk = max(VENT.bp(m)); end
  al = {};
  if pk > VENT.almPmax
    al{end + 1} = 'high Paw'; VENT.mPpk.FontColor = [1 0.33 0.44];
  else, VENT.mPpk.FontColor = [0.90 0.94 0.97]; end
  if VENT.VTe < VENT.almVtLo
    al{end + 1} = 'low VTe'; VENT.mVte.FontColor = [1 0.33 0.44];
  else, VENT.mVte.FontColor = [0.30 0.80 0.94]; end
  if PEEPi > 1.5
    al{end + 1} = 'auto-PEEP'; VENT.mPEt.FontColor = [1 0.33 0.44];
  else, VENT.mPEt.FontColor = [0.90 0.94 0.97]; end
  if isempty(al)
    VENT.almBanner.Text = '';
  else
    s = al{1};
    for i = 2:numel(al), s = [s, '    |    ', al{i}]; end
    VENT.almBanner.Text = ['ALARM:   ', s];
  end
end
%=============================================================================
function vt_loop()
  global VENT VENT_MAXTICKS
  ticks = 0;
  while ishandle(VENT.fig) && VENT.alive
    if VENT.running
      for k = 1:round(0.04 / VENT.h), vt_step(); end
      vt_trim(); vt_refresh();
    end
    drawnow();
    ticks = ticks + 1;
    if ~isempty(VENT_MAXTICKS) && ticks >= VENT_MAXTICKS, break; end
    sleep(0.04);
  end
end
%=============================================================================
