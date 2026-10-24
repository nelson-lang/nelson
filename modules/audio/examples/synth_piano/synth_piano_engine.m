%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = synth_piano_engine(midiNotes, params)
  % Render one keystroke (a note or a chord) to a stereo buffer.
  %
  % Every voice is computed in one vectorised pass: the sound is a sum of
  % partials or oscillators shaped by an ADSR envelope, then placed in the
  % stereo field by pitch (low notes left, high notes right, as seen from the
  % pianist's bench) and sent through a multi-tap echo and a diffuse
  % early-reflection "space". Nothing is streamed: the whole keystroke is
  % known in advance, which is what lets a plain audioplayer play it at once.
  %
  % params fields: preset, attack, decay, sustain, release, hold (seconds or
  % 0..1 levels), brightness, echo, space, volume (0..1), fs (Hz).
  fs = params.fs;
  total = params.attack + params.hold + params.release;
  n = max(1, round(total * fs));
  t = (0:n - 1)' / fs;
  env = synth_piano_envelope(t, params);
  y = zeros(n, 2);
  for k = 1:numel(midiNotes)
    f0 = 440 * 2 ^ ((midiNotes(k) - 69) / 12);
    voice = synthVoice(params.preset, f0, t, params.brightness, fs);
    voice = voice .* env;
    pan = min(1, max(0, (midiNotes(k) - 36) / 60));
    y = y + voice * [cos(pan * pi / 2), sin(pan * pi / 2)];
  end
  y = y / sqrt(max(1, numel(midiNotes)));
  y = synthEcho(y, fs, params.echo);
  y = synthSpace(y, fs, params.space);
  y = synthFadeOut(y, fs);
  y = tanh(1.4 * params.volume * y);
end
%=============================================================================
function v = synthVoice(preset, f0, t, brightness, fs)
  switch preset
    case 'Grand piano'
      v = synthPiano(f0, t, brightness);
    case 'Electric piano'
      v = synthElectricPiano(f0, t, brightness);
    case 'Super saw'
      v = synthSuperSaw(f0, t, brightness, fs);
    case 'Drawbar organ'
      v = synthOrgan(f0, t, brightness);
    case 'Crystal bells'
      v = synthBells(f0, t, brightness);
    case 'Chiptune'
      v = synthChiptune(f0, t, brightness);
    otherwise
      error('Nelson:synth_piano:unknownPreset', 'Unknown preset: %s', preset);
  end
end
%=============================================================================
function v = synthPiano(f0, t, brightness)
  % Stiff strings: partial h sits slightly above h*f0 (inharmonicity B) and
  % high partials die first. The unisons of a real piano string choir are
  % slightly detuned and beat slowly; one shared slow modulation stands in
  % for them at a fraction of the cost of a second set of partials.
  B = 0.00035;
  partials = max(1, min(12, floor(9000 / f0)));
  tau0 = 3.2 * (261.6 / f0) ^ 0.45;
  v = zeros(size(t));
  for h = 1:partials
    fh = h * f0 * sqrt(1 + B * h ^ 2);
    amp = (1 / h ^ (1.9 - 1.1 * brightness)) * (1 + 0.6 * sin(pi * h * 0.13));
    tau = tau0 / (1 + 0.35 * h * (1.2 - brightness));
    v = v + amp * exp(-t / tau) .* sin(2 * pi * fh * t + h);
  end
  beating = 1 + 0.18 * cos(2 * pi * 0.7 * t);
  hammer = exp(-t / 0.004) .* sin(2 * pi * 2.7 * f0 * t .* (1 + 0.5 * cos(97 * t)));
  v = 0.4 * v .* beating + 0.12 * brightness * hammer;
end
%=============================================================================
function v = synthElectricPiano(f0, t, brightness)
  % Two-operator FM: a modulation index that decays with time gives the bark
  % of the attack, and a quiet high "tine" partial rings on top.
  index = (1.5 + 3 * brightness) * exp(-t / 0.45) + 0.4;
  v = sin(2 * pi * f0 * t + index .* sin(2 * pi * f0 * t));
  tine = 0.25 * exp(-t / 0.08) .* sin(2 * pi * 14 * f0 * t);
  v = 0.45 * (v .* exp(-t / 2.2) + tine);
end
%=============================================================================
function v = synthSuperSaw(f0, t, brightness, fs)
  % Seven detuned saws spread over twenty cents - the classic trance "super
  % saw" wall - through a two-pole low-pass whose cutoff is the brightness.
  detune = [-0.11 -0.07 -0.03 0 0.03 0.07 0.11];
  phases = [0.00 0.31 0.77 0.12 0.54 0.93 0.45];
  v = zeros(size(t));
  for k = 1:numel(detune)
    fk = f0 * 2 ^ (detune(k) / 12);
    v = v + 2 * mod(fk * t + phases(k), 1) - 1;
  end
  cutoff = min(0.45, (400 + 7000 * brightness) / fs);
  pole = exp(-2 * pi * cutoff);
  v = filter((1 - pole) ^ 2, [1, -2 * pole, pole ^ 2], v);
  v = 0.09 * v;
end
%=============================================================================
function v = synthOrgan(f0, t, brightness)
  % Tonewheel drawbars 16', 8', 5 1/3', 4', 2 2/3', 2', 1', a rotating
  % speaker tremolo and the key click of the contacts.
  ratios = [0.5 1 1.5 2 3 4 8];
  levels = [0.8 1 0.6 0.7 0.45 * brightness 0.5 * brightness 0.35 * brightness];
  v = zeros(size(t));
  for k = 1:numel(ratios)
    v = v + levels(k) * sin(2 * pi * ratios(k) * f0 * t);
  end
  leslie = 1 + 0.18 * sin(2 * pi * 6.2 * t);
  click = exp(-t / 0.002) .* sin(2 * pi * 3100 * t);
  v = 0.18 * v .* leslie + 0.15 * click;
end
%=============================================================================
function v = synthBells(f0, t, brightness)
  % Inharmonic bell partials (ratios of a struck bar) with individual decays.
  ratios = [1 2.756 5.404 8.933 13.34];
  decays = [3.5 1.6 0.9 0.5 0.3];
  v = zeros(size(t));
  for k = 1:numel(ratios)
    amp = (0.5 + 0.5 * brightness) ^ (k - 1);
    v = v + amp * exp(-t / decays(k)) .* sin(2 * pi * ratios(k) * f0 * t);
  end
  v = 0.35 * v;
end
%=============================================================================
function v = synthChiptune(f0, t, brightness)
  % A 25 % pulse wave with delayed vibrato: the sound of 8-bit consoles.
  vibrato = 0.006 * min(1, t / 0.25) .* sin(2 * pi * 5.5 * t);
  phase = f0 * t .* (1 + vibrato);
  duty = 0.5 - 0.25 * brightness;
  v = 0.28 * (2 * (mod(phase, 1) < duty) - 1);
end
%=============================================================================
function y = synthEcho(y, fs, amount)
  % A ping-pong echo as a handful of explicit taps, cheaper than a long IIR
  % comb, swapping channels on every repeat. Each tap is the dry signal
  % padded in front: a whole-array add, no sub-block indexing.
  if amount <= 0
    return
  end
  delay = round(0.19 * fs);
  taps = 5;
  n = size(y, 1);
  total = n + taps * delay;
  out = [y; zeros(total - n, 2)];
  swapped = y(:, [2 1]);
  for k = 1:taps
    gain = (0.55 * amount) ^ k;
    if mod(k, 2) == 1
      source = swapped;
    else
      source = y;
    end
    out = out + [zeros(k * delay, 2); gain * source; zeros(total - n - k * delay, 2)];
  end
  y = out;
end
%=============================================================================
function y = synthSpace(y, fs, amount)
  % Diffuse early reflections: prime-numbered delays with a decaying gain,
  % crossing sides alternately so the sound opens up around the listener.
  if amount <= 0
    return
  end
  delaysMs = [23 31 41 53 67 79 97 113 131 151];
  n = size(y, 1);
  total = n + round(0.2 * fs);
  left = zeros(total, 1);
  right = zeros(total, 1);
  for k = 1:numel(delaysMs)
    d = round(delaysMs(k) * fs / 1000);
    gain = 0.45 * amount * 0.8 ^ k;
    if mod(k, 2) == 1
      right = right + [zeros(d, 1); gain * y(:, 1); zeros(total - n - d, 1)];
    else
      left = left + [zeros(d, 1); gain * y(:, 2); zeros(total - n - d, 1)];
    end
  end
  y = [y; zeros(total - n, 2)] + [left, right];
end
%=============================================================================
function y = synthFadeOut(y, fs)
  % A few milliseconds of fade on the very last samples: no click at the end.
  m = min(size(y, 1), round(0.005 * fs));
  ramp = linspace(1, 0, m)';
  y(end - m + 1:end, :) = y(end - m + 1:end, :) .* [ramp, ramp];
end
%=============================================================================
