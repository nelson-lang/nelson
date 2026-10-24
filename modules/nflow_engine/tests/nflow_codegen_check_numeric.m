%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Shared numeric-equivalence harness for generated code (not a test itself:
% no test_ prefix, so the runner does not collect it).
%
%   nflow_codegen_check_numeric(d, stem, lang, outNames, refVars, atol, rtol
%                               [, simOverrides])
%
% d        diagram struct (jsonencode-able), or a raw JSON char vector when
%          the diagram needs shapes jsonencode cannot express (1-element JSON
%          arrays like forEach partition). Every external output k must go
%          BOTH to a labelSink (isExternalPort, GotoTag = outNames{k} -- the
%          generated CSV column) and to a toWorkspace block (VariableName =
%          refVars{k} -- the simulator reference). d.sampleTime, d.duration set.
% stem     unique model stem (tempdir file/crate names derive from it).
% lang     'C' or 'rust'.
% atol/rtol  per-sample tolerance: |gen - ref| <= atol + rtol*max(|gen|,|ref|).
%
% simOverrides (optional struct): fields merged into the diagram FOR THE
%          SIMULATOR REFERENCE ONLY (e.g. struct('solver', 'dae') for
%          physical islands, whose sim backend is IDAS while the generated
%          program integrates the reduced/Newton form). Generation still
%          sees the original diagram.
%
% The caller guards with skip_testsuite(~__nflow_codegen_toolchain__(lang), ...).
% Pipeline: simulate via __nflow_simulate__ (reference trajectories from the
% base workspace), nflow_codegenerate with build=true (Release), run the
% generated nflow_run binary, parse its CSV and compare sample-by-sample on
% the common index range (both sides step t = 0 : dt : duration).
%=============================================================================
function nflow_codegen_check_numeric(d, stem, lang, outNames, refVars, atol, rtol, simOverrides)
  if ischar(d)
    dj = d;
    d = jsondecode(dj);
  else
    dj = jsonencode(d);
  end
  model = [tempdir(), stem, '.nflow'];
  fid = fopen(model, 'wt');
  fwrite(fid, dj);
  fclose(fid);

  % --- Reference: interpreter trajectories via toWorkspace ------------------
  % Overrides are patched TEXTUALLY (a jsondecode/jsonencode round-trip of
  % the diagram would reshape heterogeneous arrays like island components).
  simDj = dj;
  if nargin > 7 && isstruct(simOverrides)
    fns = fieldnames(simOverrides);
    for k = 1:numel(fns)
      v = simOverrides.(fns{k});
      assert_istrue(ischar(v));
      key = ['"', fns{k}, '":"'];
      assert_istrue(~isempty(strfind(simDj, key)));
      simDj = regexprep(simDj, [key, '[a-zA-Z0-9_.+-]*"'], [key, v, '"']);
    end
  end
  r = jsondecode(__nflow_simulate__(simDj));
  assert_isequal(r.status, 'done');
  refs = cell(1, numel(refVars));
  for k = 1:numel(refVars)
    refs{k} = evalin('base', refVars{k});
    refs{k} = refs{k}(:);
    evalin('base', ['clear(''', refVars{k}, ''')']);
  end

  % --- Generate + build + run ----------------------------------------------
  % Start from a clean crate directory: the same stem serves the C and Rust
  % legs, and stale build trees (cargo target/, CMake caches) both bloat the
  % tempdir across runs and risk a hybrid directory.
  crate = [tempdir(), stem];
  if isdir(crate)
    try
      rmdir(crate, 's');
    catch
    end
  end
  nflow_codegenerate(model, tempdir(), true, lang, ...
    struct('build', true, 'buildConfig', 'Release'));
  if strcmp(lang, 'rust')
    exe = [crate, '/target/release/nflow_run'];
  else
    if ispc()
      exe = [crate, '/build/Release/nflow_run'];
    else
      exe = [crate, '/build/nflow_run'];
    end
  end
  if ispc()
    exe = [exe, '.exe'];
  end
  assert_istrue(isfile(exe));
  csv = [tempdir(), stem, '_', lang, '.csv'];
  [st, out] = system(['"', exe, '" -t ', num2str(d.duration), ' -o "', csv, '"']);
  assert_isequal(st, 0, out);
  assert_istrue(isfile(csv));

  % --- Parse the CSV (t,<outNames...>) -------------------------------------
  txt = fileread(csv);
  lines = strsplit(strtrim(txt), sprintf('\n'));
  assert_istrue(numel(lines) >= 2);
  header = strsplit(strtrim(lines{1}), ',');
  assert_isequal(header{1}, 't');
  nrows = numel(lines) - 1;
  data = zeros(nrows, numel(header));
  for i = 1:nrows
    vals = str2double(strsplit(strtrim(lines{i + 1}), ','));
    assert_isequal(numel(vals), numel(header));
    data(i, :) = vals;
  end

  % --- Time-grid check ------------------------------------------------------
  dt = d.sampleTime;
  tref = (0:(nrows - 1))' * dt;
  assert_istrue(max(abs(data(:, 1) - tref)) <= 1e-9);

  % --- Column-by-column comparison on the common range ----------------------
  for k = 1:numel(outNames)
    col = 0;
    for h = 2:numel(header)
      if strcmp(header{h}, outNames{k})
        col = h;
        break;
      end
    end
    assert_istrue(col > 0);
    gen = data(:, col);
    ref = refs{k};
    n = min(numel(gen), numel(ref));
    assert_istrue(n >= 2);
    g = gen(1:n);
    f = ref(1:n);
    tol = atol + rtol .* max(abs(g), abs(f));
    err = abs(g - f);
    if any(err > tol)
      bad = find(err > tol, 1);
      error('nflow_codegen_check_numeric: %s[%d] gen=%.9g ref=%.9g err=%.3g > tol=%.3g (%s)', ...
        outNames{k}, bad, g(bad), f(bad), err(bad), tol(bad), lang);
    end
  end
  rmfile(model);
end
%=============================================================================
