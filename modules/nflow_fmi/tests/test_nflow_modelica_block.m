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
% OpenModelica bridge: a `modelica` block in a model is expanded
% by sim() into an `fmu` block (compiled with OpenModelica) and simulated.
% Where OpenModelica is capable this runs a first-order model to e^{-t}; where
% it is not, sim() blocks with the typed guard. The rewrite is surgical, so a
% sibling block whose parameters merely mention "modelica" is preserved.
%=============================================================================
function p = mb_write(doc)
  p = [tempname(), '.nflow'];
  fid = fopen(p, 'w'); fwrite(fid, jsonencode(doc)); fclose(fid);
end
%=============================================================================
mo = sprintf('model FO\n  output Real x(start = 1.0);\nequation\n  der(x) = -x;\nend FO;\n');
m = struct('id', 'm', 'type', 'modelica', 'inputs', 0, 'outputs', 1, ...
            'params', struct('source', mo, 'modelName', 'FO'));
tw = struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
            'params', struct('VariableName', 'y'));
conn = struct('from', 'm', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0);
doc = struct('blocks', {{m, tw}}, 'connections', {{conn}}, 'runtime', 1.0, 'sampleTime', 0.05);
%=============================================================================
info = modelicaInfo();
if ~info.capable
  % Guard: the Modelica block blocks the simulation with a typed error.
  id = '';
  try
    sim(mb_write(doc));
  catch e
    id = e.identifier;
  end
  assert_isequal(id, 'Nelson:nflow_fmi:modelicaUnavailable');
else
  % Happy path: sim() compiles the Modelica model and simulates x(t) = e^{-t}.
  out = sim(mb_write(doc));
  y = out.logsout(1).values;
  assert_istrue(abs(y(1) - 1.0) < 1e-6);
  assert_istrue(abs(y(end) - exp(-1)) < 2e-2);

  % Surgical rewrite: a sibling whose params merely mention the words "modelica"
  % and "blocks" is preserved and still simulates.
  note = 'decoy mentions modelica and "blocks":[9] inside {a string}';
  c = struct('id', 'c', 'type', 'constant', 'inputs', 0, 'outputs', 1, ...
               'params', struct('Value', 7, 'note', note));
  tw2 = struct('id', 'tw2', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
               'params', struct('VariableName', 'y2'));
  c2 = struct('from', 'c', 'to', 'tw2', 'fromIndex', 0, 'toIndex', 0);
  doc2 = struct('blocks', {{c, m, tw, tw2}}, ...
                'connections', {{conn, c2}}, 'runtime', 1.0, 'sampleTime', 0.05);
  out2 = sim(mb_write(doc2));
  names = {out2.logsout.name};
  y1 = out2.logsout(find(strcmp(names, 'y'), 1)).values;
  y2 = out2.logsout(find(strcmp(names, 'y2'), 1)).values;
  assert_istrue(abs(y1(end) - exp(-1)) < 2e-2); % modelica expanded
  assert_istrue(abs(y2(end) - 7) < 1e-9); % decoy preserved

  % Regression: a block that references a .mo FILE while its inline `source`
  % param is present-but-empty (the editor defaults both source and file to "").
  % The empty source must NOT shadow the file -- it did, failing with
  % "block has no source" even though the file was set (the GUI Run bug).
  % The file base name matches the model class (FO), as in a real .mo file, so
  % the empty model name is derived from it.
  moDir = tempname(); mkdir(moDir);
  cMo = onCleanup(@() rmdir(moDir, 's')); %#ok<NASGU>
  moPath = fullfile(moDir, 'FO.mo');
  fid = fopen(moPath, 'w'); fwrite(fid, mo); fclose(fid);
  mf = struct('id', 'mf', 'type', 'modelica', 'inputs', 0, 'outputs', 1, ...
              'params', struct('source', '', 'file', moPath, 'modelName', ''));
  connf = struct('from', 'mf', 'to', 'tw', 'fromIndex', 0, 'toIndex', 0);
  docf = struct('blocks', {{mf, tw}}, 'connections', {{connf}}, ...
                'runtime', 1.0, 'sampleTime', 0.05);
  outf = sim(mb_write(docf));
  yf = outf.logsout(1).values;
  assert_istrue(abs(yf(1) - 1.0) < 1e-6);
  assert_istrue(abs(yf(end) - exp(-1)) < 2e-2); % file used despite empty source
end
%=============================================================================
