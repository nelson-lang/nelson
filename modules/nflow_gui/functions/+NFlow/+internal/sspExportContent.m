%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% sspPath = sspExportContent(nflowContent, sspFile)
%
% Export a diagram (given as its .nflow JSON content) to an SSP archive and
% return the produced path. Internal RPC helper invoked by the editor's
% Export-as-SSP action from the interpreter thread.
%
% Two cases, decided from the diagram:
%   * a composition of FMU blocks (e.g. one opened from an SSP) is repackaged
%     directly via NFlow.sspExportDiagram (no recompile) -- the inverse of Open
%     SSP;
%   * any other (native) diagram is exported as a single component: the whole
%     model is compiled to one FMI 3.0 FMU (NFlow.sspExport) and wrapped in the
%     SSP, so any nflow model can become an SSP.
% See NFlow.sspImport, which does the inverse for File > Open SSP.
%=============================================================================
function sspPath = sspExportContent(nflowContent, sspFile)
  narginchk(2, 2);
  tmp = [tempname(), '.nflow'];
  fid = fopen(tmp, 'wt');
  if fid == -1
    error('Nelson:nflow_gui:sspExport', 'Could not stage a temporary model for SSP export.');
  end
  fwrite(fid, nflowContent);
  fclose(fid);
  cleanup = onCleanup(@() ssp_export_delete(tmp)); %#ok<NASGU>
  if ssp_export_has_fmu(nflowContent)
    % A composition of FMU blocks: repackage the referenced FMUs directly.
    sspPath = NFlow.sspExportDiagram(tmp, sspFile);
  else
    % A native diagram: compile the whole model to one FMU component and expose
    % its external ports (labelSource inputs / labelSink outputs) as the SSP's
    % system connectors, so the composition has an observable interface (an
    % output connector, so a re-import records something instead of 'no_scope').
    % Boolean/integer boundary signals are cast to Float64 by the FMU export.
    [~, compName] = fileparts(sspFile);
    compName = regexprep(compName, '[^A-Za-z0-9_]', '_');
    if isempty(compName)
      compName = 'Model';
    end
    component = struct('name', compName, 'model', tmp);
    [inNames, outNames] = ssp_export_boundary(nflowContent);
    conns = struct('startElement', {}, 'startConnector', {}, ...
      'endElement', {}, 'endConnector', {});
    for i = 1:numel(outNames)
      conns(end + 1) = struct('startElement', compName, 'startConnector', outNames{i}, ...
        'endElement', '', 'endConnector', outNames{i}); %#ok<AGROW>
    end
    for i = 1:numel(inNames)
      conns(end + 1) = struct('startElement', '', 'startConnector', inNames{i}, ...
        'endElement', compName, 'endConnector', inNames{i}); %#ok<AGROW>
    end
    if isempty(conns)
      sspPath = NFlow.sspExport(sspFile, component);
    else
      sspPath = NFlow.sspExport(sspFile, component, conns);
    end
  end
end
%=============================================================================
function [inNames, outNames] = ssp_export_boundary(nflowContent)
  % External connector names: labelSource GotoTags are inputs, labelSink GotoTags
  % are outputs (the FMU export names the FMU ports after these tags).
  inNames = {};
  outNames = {};
  try
    doc = jsondecode(nflowContent);
  catch
    return
  end
  if ~isfield(doc, 'blocks')
    return
  end
  blocks = doc.blocks;
  for k = 1:numel(blocks)
    if iscell(blocks)
      b = blocks{k};
    else
      b = blocks(k);
    end
    if ~isfield(b, 'type') || ~isfield(b, 'params')
      continue
    end
    if ~isfield(b.params, 'GotoTag')
      continue
    end
    tag = char(b.params.GotoTag);
    if isempty(tag)
      continue
    end
    if strcmp(b.type, 'labelSource')
      inNames{end + 1} = tag; %#ok<AGROW>
    elseif strcmp(b.type, 'labelSink')
      outNames{end + 1} = tag; %#ok<AGROW>
    end
  end
end
%=============================================================================
function tf = ssp_export_has_fmu(nflowContent)
  tf = false;
  % Cheap pre-check before decoding.
  if isempty(strfind(nflowContent, '"fmu"'))
    return
  end
  try
    doc = jsondecode(nflowContent);
  catch
    return
  end
  if ~isfield(doc, 'blocks')
    return
  end
  blocks = doc.blocks;
  for k = 1:numel(blocks)
    if iscell(blocks)
      b = blocks{k};
    else
      b = blocks(k);
    end
    if isfield(b, 'type') && strcmp(b.type, 'fmu')
      tf = true;
      return
    end
  end
end
%=============================================================================
function ssp_export_delete(p)
  if isfile(p)
    delete(p);
  end
end
%=============================================================================
