%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef NFlow
  % NFlow  Namespace for the nflow programmatic model API.
  methods (Static)
    function connection = connectBlocks(src, dst, varargin)
      % NFlow.connectBlocks  Connect the first free ports of two blocks.
      %   connection = NFlow.connectBlocks(src, dst)
      %   connection = NFlow.connectBlocks(src, dst, 'RoutingStyle', style)
      connection = __nflow_connectBlocks__(src, dst, varargin{:});
    end

    function paths = addlibrarypath(folder)
      % NFlow.addlibrarypath  Add an nflow block library search path.
      %   NFlow.addlibrarypath(folder)
      %   paths = NFlow.addlibrarypath(folder)
      %
      %   folder is a directory that holds one or more library sub-folders,
      %   each with a library.json and its native library. The folder is added
      %   to the search paths and scanned, so its blocks become available to
      %   sim, code generation and the editor without restarting Nelson.
      %
      %   With an output, paths is the updated list of search paths (a cell
      %   array of char).
      narginchk(1, 1);
      if nargout == 0
        __nflow_addlibrarypath__(folder);
      else
        paths = __nflow_addlibrarypath__(folder);
      end
    end

    function paths = librarypaths()
      % NFlow.librarypaths  Return the nflow block library search paths.
      %   paths = NFlow.librarypaths()
      paths = __nflow_librarypaths__();
    end

    function paths = rmlibrarypath(folder)
      % NFlow.rmlibrarypath  Remove an nflow block library search path.
      %   NFlow.rmlibrarypath(folder)
      %   paths = NFlow.rmlibrarypath(folder)
      %
      %   With an output, paths is the updated list of search paths.
      narginchk(1, 1);
      if nargout == 0
        __nflow_rmlibrarypath__(folder);
      else
        paths = __nflow_rmlibrarypath__(folder);
      end
    end

    function figures = plotScopes(out)
      % NFlow.plotScopes  Open one figure per scope of a simulation result.
      %   figures = NFlow.plotScopes(out)
      %
      %   out is the structure returned by sim(). Every logged signal in
      %   out.logsout is drawn in its own figure, one line per channel. Handy
      %   as a model StopFcn, e.g. set the model's StopFcn to
      %   'NFlow.plotScopes(out)' to pop the traces up when the run stops.
      narginchk(1, 1);
      figures = [];
      if ~isstruct(out) || ~isfield(out, 'logsout')
        return
      end
      logs = out.logsout;
      for k = 1:numel(logs)
        values = logs(k).values;
        if isempty(values)
          continue
        end
        t = logs(k).time(:);
        if isempty(t) || numel(t) ~= size(values, 1)
          t = (1:size(values, 1))';
        end
        f = figure();
        plot(t, values, 'LineWidth', 1.4);
        grid('on');
        xlabel('t (s)');
        ylabel('signal');
        title(['scope: ', logs(k).name]);
        channels = size(values, 2);
        legendLabels = cell(1, channels);
        for j = 1:channels
          legendLabels{j} = sprintf('in %d', j);
        end
        legend(legendLabels, 'Location', 'best');
        figures(end + 1) = f; %#ok<AGROW>
      end
    end

    function fmuPath = exportfmu(modelFile, destinationDirectory)
      % NFlow.exportfmu  Export a model as an FMI 3.0 Co-Simulation source FMU.
      %   fmuPath = NFlow.exportfmu(modelFile)
      %   fmuPath = NFlow.exportfmu(modelFile, destinationDirectory)
      %
      %   Generates the model to C, wraps it with an FMI 3.0 Co-Simulation
      %   interface, and packages modelDescription.xml + the C sources into a
      %   <model>.fmu archive. Returns the full path to the .fmu.
      %
      %   Only Float64 signals are supported at the FMU boundary; conditional-
      %   execution and FMU / nelsonFunction blocks are rejected.
      narginchk(1, 2);
      if ~ischar(modelFile) && ~(isstring(modelFile) && isscalar(modelFile))
        error('Nelson:nflow_engine:invalidArgument:firstArgumentANflowModelFile', 'First argument must be a .nflow model file path.');
      end
      modelFile = char(modelFile);
      if ~isfile(modelFile)
        error('Nelson:nflow_engine:modelFileNotFound', 'The model file does not exist: %s', modelFile);
      end
      if nargin < 2 || isempty(destinationDirectory)
        destinationDirectory = fileparts(modelFile);
        if isempty(destinationDirectory)
          destinationDirectory = pwd();
        end
      end
      destinationDirectory = char(destinationDirectory);
      if ~isdir(destinationDirectory)
        mkdir(destinationDirectory);
      end
      if ispc()
        try
          loadcompilerconf();
        catch
        end
      end
      % Stage the FMU tree (modelDescription.xml + sources/) in a temp directory.
      staging = tempname();
      mkdir(staging);
      % When the FMI 3.0 headers are available (nflow_fmi module) and a C
      % toolchain is present, the sources are also compiled to a platform binary
      % under binaries/<platform>/; otherwise a source-only FMU is produced.
      fmiInclude = '';
      try
        % Prefer the shipped copy (installed with the module); fall back to the
        % vendored fmi4c sources in a development tree.
        candidates = {[modulepath('nflow_fmi'), '/resources/fmi3'], ...
          [modulepath('nflow_fmi'), '/src/fmi4c/3rdparty/fmi']};
        for ci = 1:numel(candidates)
          if isdir(candidates{ci})
            fmiInclude = candidates{ci};
            break;
          end
        end
      catch
      end
      modelName = __nflow_export_fmu_sources__(modelFile, staging, fmiInclude);
      fmuPath = fullfile(destinationDirectory, [modelName, '.fmu']);
      if isfile(fmuPath)
        delete(fmuPath);
      end
      % A .fmu is a zip whose root holds modelDescription.xml and sources/ (and
      % binaries/ when the binary was built). Pass absolute staged files with a
      % trailing-separator root so the archive entries are relative to the
      % staging directory (no staging prefix).
      files = {fullfile(staging, 'modelDescription.xml'), fullfile(staging, 'sources')};
      binDir = fullfile(staging, 'binaries');
      if isdir(binDir)
        files{end + 1} = binDir;
      end
      zip(fmuPath, files, [staging, filesep]);
    end

    function info = sspInfo(sspFile)
      % NFlow.sspInfo  Inspect an SSP (System Structure and Parameterization) file.
      %   info = NFlow.sspInfo(sspFile)
      %
      %   Reads the SystemStructure.ssd of an .ssp archive and returns a struct:
      %     info.name         top-level system name
      %     info.components   struct array {name, source, type, implementation}
      %                       (each component references an FMU under resources/)
      %     info.connections  struct array {startElement, startConnector,
      %                       endElement, endConnector}
      narginchk(1, 1);
      sspFile = char(sspFile);
      if ~isfile(sspFile)
        error('Nelson:nflow_engine:sspNotFound', 'SSP file not found: %s', sspFile);
      end
      staging = tempname();
      mkdir(staging);
      unzip(sspFile, staging);
      ssd = fullfile(staging, 'SystemStructure.ssd');
      if ~isfile(ssd)
        error('Nelson:nflow_engine:invalidSSP', 'Invalid SSP: SystemStructure.ssd not found at the archive root.');
      end
      doc = xmlread(ssd);
      root = doc.getDocumentElement();
      info = struct();
      info.name = char(root.getAttribute('name'));

      % Map each component name to the .ssv files it binds. This DOM offers no
      % tree navigation (no getParentNode, and a scoped getElementsByTagName on an
      % element returns nothing), so the Component<->ParameterBinding association
      % is recovered from the .ssd text: a binding belongs to the nearest
      % Component element that opens before it.
      ssdText = fileread(ssd);
      bindMap = NFlow.componentBindings(ssdText);
      info.systemConnectors = NFlow.systemConnectors(ssdText);

      comps = doc.getElementsByTagName('Component');
      components = struct('name', {}, 'source', {}, 'type', {}, ...
        'implementation', {}, 'parameterSources', {}, 'inlineParameters', {});
      for i = 0:(comps.getLength() - 1)
        c = comps.item(i);
        nm = char(c.getAttribute('name'));
        srcs = {};
        inlinePv = struct();
        bi = find(strcmp({bindMap.name}, nm), 1);
        if ~isempty(bi)
          srcs = bindMap(bi).sources;
          % Parameter values written straight into the .ssd (a ParameterBinding
          % with no source but an inline ParameterSet) are parsed here.
          for k = 1:numel(bindMap(bi).inlineSets)
            inlinePv = NFlow.mergeStruct(inlinePv, ...
              NFlow.readSsvText(bindMap(bi).inlineSets{k}));
          end
        end
        components(end + 1) = struct( ...
          'name', nm, ...
          'source', char(c.getAttribute('source')), ...
          'type', char(c.getAttribute('type')), ...
          'implementation', char(c.getAttribute('implementation')), ...
          'parameterSources', {srcs}, ...
          'inlineParameters', inlinePv); %#ok<AGROW>
      end
      info.components = components;

      conns = doc.getElementsByTagName('Connection');
      connections = struct('startElement', {}, 'startConnector', {}, 'endElement', {}, 'endConnector', {});
      for i = 0:(conns.getLength() - 1)
        k = conns.item(i);
        connections(end + 1) = struct( ...
          'startElement', char(k.getAttribute('startElement')), ...
          'startConnector', char(k.getAttribute('startConnector')), ...
          'endElement', char(k.getAttribute('endElement')), ...
          'endConnector', char(k.getAttribute('endConnector'))); %#ok<AGROW>
      end
      info.connections = connections;
    end

    function nflowPath = sspImport(sspFile, destinationDirectory)
      % NFlow.sspImport  Import an SSP into a runnable .nflow model.
      %   nflowPath = NFlow.sspImport(sspFile)
      %   nflowPath = NFlow.sspImport(sspFile, destinationDirectory)
      %
      %   Extracts the SSP's component FMUs, maps each component to an FMU block
      %   and each connection to a wire (connectors matched to ports by name via
      %   native FMU inspection), and writes a .nflow model that simulates like
      %   any diagram. Component FMUs must be FMI 2.0 or 3.0 (FMI 1.0 is not
      %   supported); an FMU that cannot be inspected is reported.
      narginchk(1, 2);
      sspFile = char(sspFile);
      if ~isfile(sspFile)
        error('Nelson:nflow_engine:sspNotFound', 'SSP file not found: %s', sspFile);
      end
      if nargin < 2 || isempty(destinationDirectory)
        destinationDirectory = fileparts(sspFile);
        if isempty(destinationDirectory)
          destinationDirectory = pwd();
        end
      end
      destinationDirectory = char(destinationDirectory);
      if ~isdir(destinationDirectory)
        mkdir(destinationDirectory);
      end
      info = NFlow.sspInfo(sspFile);
      [~, stem] = fileparts(sspFile);
      stem = regexprep(stem, '[^a-zA-Z0-9_]', '_');
      % Extract the SSP resources next to the produced model so the FMU blocks
      % resolve their sources.
      resDir = fullfile(destinationDirectory, [stem, '_resources']);
      if isdir(resDir)
        rmdir(resDir, 's');
      end
      mkdir(resDir);
      unzip(sspFile, resDir);

      blocks = {};
      % Per component: its block id and ordered input/output connector names.
      compIndex = struct('name', {}, 'blockId', {}, 'inputs', {}, 'outputs', {});
      for i = 1:numel(info.components)
        comp = info.components(i);
        fmuPath = fullfile(resDir, strrep(comp.source, '/', filesep));
        if ~isfile(fmuPath)
          error('Nelson:nflow_engine:sspFmuNotFound', 'SSP component FMU not found: %s', comp.source);
        end
        j = jsondecode(__nflow_fmi_inspect_json__(fmuPath));
        if ~isfield(j, 'success') || ~j.success
          % Not loadable: most commonly FMI 1.0 (the runtime supports 2.0 and
          % 3.0) or a corrupt FMU. Report the version, read from the inspector
          % when exposed, otherwise straight from the modelDescription.xml.
          ver = '';
          if isfield(j, 'fmiVersion')
            ver = char(j.fmiVersion);
          else
            ver = NFlow.fmuFmiVersion(fmuPath);
          end
          if ~isempty(ver) && ~strcmp(ver, '2.0') && ~strcmp(ver, '3.0')
            error('Nelson:nflow_engine:unsupportedFmiVersion', 'SSP component requires FMI 2.0 or 3.0 but is FMI %s: %s', ver, comp.source);
          end
          error('Nelson:nflow_engine:fmuLoadFailed', 'Cannot load SSP component FMU (FMI 2.0 or 3.0 Co-Simulation required): %s', comp.source);
        end
        inNames = {}; outNames = {};
        % jsondecode yields a struct array when every variable has the same
        % fields, and a cell array when they differ (outputs have no 'start').
        vars = j.variables;
        for v = 1:numel(vars)
          if iscell(vars)
            var = vars{v};
          else
            var = vars(v);
          end
          if strcmp(var.causality, 'input')
            inNames{end + 1} = var.name; %#ok<AGROW>
          elseif strcmp(var.causality, 'output')
            outNames{end + 1} = var.name; %#ok<AGROW>
          end
        end
        blockId = regexprep(comp.name, '[^a-zA-Z0-9_]', '_');
        params = struct('path', fmuPath);
        % SSP parameterization: apply the values from every .ssv this component
        % binds, so the FMU block starts with the composition's parameters.
        pv = struct();
        if isfield(comp, 'parameterSources')
          for s = 1:numel(comp.parameterSources)
            ssvPath = fullfile(resDir, strrep(comp.parameterSources{s}, '/', filesep));
            if isfile(ssvPath)
              pv = NFlow.mergeStruct(pv, NFlow.readSsv(ssvPath));
            end
          end
        end
        % Values embedded directly in the .ssd take effect alongside any .ssv.
        if isfield(comp, 'inlineParameters')
          pv = NFlow.mergeStruct(pv, comp.inlineParameters);
        end
        if ~isempty(fieldnames(pv))
          params.parameterValues = pv;
        end
        % Port layout for the FMU block renderer: without numInputs/numOutputs
        % and the port names, the block draws a single default port at its origin
        % (a stray marker in the top-left corner) instead of one labelled port per
        % connector. These are the same fields FMU inspection writes.
        params.numInputs = numel(inNames);
        params.numOutputs = numel(outNames);
        params.inputNames = inNames;
        params.outputNames = outNames;
        blocks{end + 1} = struct('id', blockId, 'type', 'fmu', ...
          'inputs', numel(inNames), 'outputs', max(1, numel(outNames)), ...
          'params', params); %#ok<AGROW>
        compIndex(end + 1) = struct('name', comp.name, 'blockId', blockId, ...
          'inputs', {inNames}, 'outputs', {outNames}); %#ok<AGROW>
      end

      findComp = @(nm) find(strcmp({compIndex.name}, nm), 1);
      connections = {};
      for i = 1:numel(info.connections)
        cn = info.connections(i);
        si = findComp(cn.startElement);
        di = findComp(cn.endElement);
        if ~isempty(cn.startElement) && ~isempty(cn.endElement)
          % Component-to-component: wire output port -> input port by name.
          if isempty(si) || isempty(di)
            continue;
          end
          fromIdx = find(strcmp(compIndex(si).outputs, cn.startConnector), 1);
          toIdx = find(strcmp(compIndex(di).inputs, cn.endConnector), 1);
          if isempty(fromIdx) || isempty(toIdx)
            continue; % connector not found on that component's ports
          end
          connections{end + 1} = struct('from', compIndex(si).blockId, ...
            'fromIndex', fromIdx - 1, 'to', compIndex(di).blockId, ...
            'toIndex', toIdx - 1); %#ok<AGROW>
        elseif ~isempty(cn.startElement) && isempty(cn.endElement) && ~isempty(si)
          % Component output -> parent system output: expose it as a recordable
          % To Workspace sink named after the system connector, so the imported
          % model produces the composition's observable outputs.
          fromIdx = find(strcmp(compIndex(si).outputs, cn.startConnector), 1);
          if isempty(fromIdx)
            continue;
          end
          sinkVar = regexprep(cn.endConnector, '[^a-zA-Z0-9_]', '_');
          sinkId = ['out_', sinkVar];
          blocks{end + 1} = struct('id', sinkId, 'type', 'toWorkspace', ...
            'inputs', 1, 'outputs', 0, 'params', struct('VariableName', sinkVar, ...
            'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf')); %#ok<AGROW>
          connections{end + 1} = struct('from', compIndex(si).blockId, ...
            'fromIndex', fromIdx - 1, 'to', sinkId, 'toIndex', 0); %#ok<AGROW>
        end
        % A parent-system input -> component input (empty startElement) has no
        % driver in the imported model; left unconnected (the FMU input holds 0).
      end

      % Lay the blocks out so the imported model is readable instead of stacked
      % at the origin (blocks created above carry no x/y). Signal flow is
      % left-to-right: FMU components in left column(s), To Workspace output
      % sinks in a column to their right; each column stacks vertically, wrapping
      % to a new column after a few rows so a large composition stays compact.
      x0 = 80; yTop = 80; dx = 240; dy = 150; rowsPerCol = 5;
      rFmu = 0;
      for bi = 1:numel(blocks)
        if strcmp(blocks{bi}.type, 'toWorkspace')
          continue % sinks placed after, to the right of every component column
        end
        col = floor(rFmu / rowsPerCol);
        row = mod(rFmu, rowsPerCol);
        blocks{bi}.x = x0 + col * dx;
        blocks{bi}.y = yTop + row * dy;
        blocks{bi}.rotation = 0;
        rFmu = rFmu + 1;
      end
      fmuCols = max(1, ceil(rFmu / rowsPerCol));
      sinkX = x0 + fmuCols * dx + 120;
      rSink = 0;
      for bi = 1:numel(blocks)
        if ~strcmp(blocks{bi}.type, 'toWorkspace')
          continue
        end
        blocks{bi}.x = sinkX + floor(rSink / rowsPerCol) * dx;
        blocks{bi}.y = yTop + mod(rSink, rowsPerCol) * dy;
        blocks{bi}.rotation = 0;
        rSink = rSink + 1;
      end

      d = struct();
      d.blocks = blocks;
      d.connections = connections;
      d.sampleTime = 0.01;
      d.duration = 10.0;
      d.variables = struct();
      d.solver = 'discrete';
      nflowPath = fullfile(destinationDirectory, [stem, '.nflow']);
      fid = fopen(nflowPath, 'wt');
      fwrite(fid, jsonencode(d));
      fclose(fid);
    end

    function sspPath = sspExport(sspFile, components, connections)
      % NFlow.sspExport  Compose nflow models into an SSP archive.
      %   sspPath = NFlow.sspExport(sspFile, components)
      %   sspPath = NFlow.sspExport(sspFile, components, connections)
      %
      %   components : struct array with fields
      %       .name   component name (used for resources/<name>.fmu)
      %       .model  path to a .nflow model, exported to an FMI 3.0 FMU
      %   connections (optional) : struct array with fields
      %       .startElement .startConnector .endElement .endConnector
      %       (connectors are the models' external label-port names)
      %
      %   Each model is exported to an FMU, placed under resources/, listed as a
      %   Component (with its inspected input/output Connectors) in
      %   SystemStructure.ssd, and the whole tree is zipped into the .ssp.
      narginchk(2, 3);
      if nargin < 3
        connections = struct('startElement', {}, 'startConnector', {}, 'endElement', {}, 'endConnector', {});
      end
      sspFile = char(sspFile);
      staging = tempname();
      mkdir(staging);
      resDir = fullfile(staging, 'resources');
      mkdir(resDir);

      esc = @(s) strrep(strrep(strrep(strrep(char(s), '&', '&amp;'), '<', '&lt;'), '>', '&gt;'), '"', '&quot;');
      body = '';
      for i = 1:numel(components)
        comp = components(i);
        cname = char(comp.name);
        fmu = NFlow.exportfmu(char(comp.model), resDir);
        % Name the component FMU after the component (stable source path).
        target = fullfile(resDir, [cname, '.fmu']);
        if ~strcmp(fmu, target)
          copyfile(fmu, target);
          delete(fmu);
        end
        % Inspect for the connector list (ports by causality).
        connectors = '';
        try
          j = jsondecode(__nflow_fmi_inspect_json__(target));
          if isfield(j, 'variables')
            vars = j.variables;
            for v = 1:numel(vars)
              if iscell(vars)
                var = vars{v};
              else
                var = vars(v);
              end
              if strcmp(var.causality, 'input') || strcmp(var.causality, 'output')
                connectors = [connectors, ...
                  '        <ssd:Connector name="', esc(var.name), '" kind="', var.causality, '">', char(10), ...
                  '          <ssc:Real/>', char(10), ...
                  '        </ssd:Connector>', char(10)]; %#ok<AGROW>
              end
            end
          end
        catch
        end
        body = [body, ...
          '      <ssd:Component name="', esc(cname), '" source="resources/', esc(cname), '.fmu"', ...
          ' type="application/x-fmu-sharedlibrary" implementation="CoSimulation">', char(10), ...
          '        <ssd:Connectors>', char(10), connectors, '        </ssd:Connectors>', char(10), ...
          '      </ssd:Component>', char(10)]; %#ok<AGROW>
      end

      connXml = '';
      for i = 1:numel(connections)
        cn = connections(i);
        connXml = [connXml, ...
          '      <ssd:Connection startElement="', esc(char(cn.startElement)), ...
          '" startConnector="', esc(char(cn.startConnector)), ...
          '" endElement="', esc(char(cn.endElement)), ...
          '" endConnector="', esc(char(cn.endConnector)), '"/>', char(10)]; %#ok<AGROW>
      end

      % Connections that reference the parent system (an empty start/end element)
      % define the composition's own connectors: an empty startElement is a
      % system input named after startConnector, an empty endElement a system
      % output named after endConnector. Declare them so the SSP is self-describing.
      sysConn = struct('name', {}, 'kind', {});
      for i = 1:numel(connections)
        cn = connections(i);
        if isempty(char(cn.startElement)) && ~isempty(char(cn.startConnector))
          sysConn = NFlow.addConnector(sysConn, char(cn.startConnector), 'input');
        end
        if isempty(char(cn.endElement)) && ~isempty(char(cn.endConnector))
          sysConn = NFlow.addConnector(sysConn, char(cn.endConnector), 'output');
        end
      end
      sysConnXml = '';
      for i = 1:numel(sysConn)
        sysConnXml = [sysConnXml, ...
          '      <ssd:Connector name="', esc(sysConn(i).name), '" kind="', sysConn(i).kind, '">', char(10), ...
          '        <ssc:Real/>', char(10), ...
          '      </ssd:Connector>', char(10)]; %#ok<AGROW>
      end
      sysConnBlock = '';
      if ~isempty(sysConnXml)
        sysConnBlock = ['    <ssd:Connectors>', char(10), sysConnXml, '    </ssd:Connectors>', char(10)];
      end

      [~, sysName] = fileparts(sspFile);
      ssd = ['<?xml version="1.0" encoding="UTF-8"?>', char(10), ...
        '<ssd:SystemStructureDescription version="1.0" name="', esc(sysName), '"', ...
        ' xmlns:ssd="http://ssp-standard.org/SSP1/SystemStructureDescription"', ...
        ' xmlns:ssc="http://ssp-standard.org/SSP1/SystemStructureCommon">', char(10), ...
        '  <ssd:System name="', esc(sysName), '">', char(10), ...
        sysConnBlock, ...
        '    <ssd:Elements>', char(10), body, '    </ssd:Elements>', char(10), ...
        '    <ssd:Connections>', char(10), connXml, '    </ssd:Connections>', char(10), ...
        '  </ssd:System>', char(10), ...
        '</ssd:SystemStructureDescription>', char(10)];
      fid = fopen(fullfile(staging, 'SystemStructure.ssd'), 'wt');
      fwrite(fid, ssd);
      fclose(fid);

      if isfile(sspFile)
        delete(sspFile);
      end
      zip(sspFile, {fullfile(staging, 'SystemStructure.ssd'), resDir}, [staging, filesep]);
      sspPath = sspFile;
    end

    function sspPath = sspExportDiagram(nflowFile, sspFile)
      % NFlow.sspExportDiagram  Package a diagram of FMU blocks into an SSP.
      %   sspPath = NFlow.sspExportDiagram(nflowFile, sspFile)
      %
      %   The inverse of NFlow.sspImport: reads a .nflow model whose blocks are
      %   FMU blocks (each referencing a component FMU) and re-emits an SSP that
      %   references those same FMUs directly (no re-export). Each 'fmu' block
      %   becomes a Component, wires between FMU blocks become Connections
      %   (port indices mapped back to connector names by native inspection), and
      %   every 'toWorkspace' sink fed by an FMU output becomes a system output
      %   connector. SSP <-> nflow round-trips: sspImport then sspExportDiagram
      %   reproduces the composition's components and connections.
      narginchk(2, 2);
      nflowFile = char(nflowFile);
      sspFile = char(sspFile);
      if ~isfile(nflowFile)
        error('Nelson:nflow_engine:modelFileNotFound', 'The model file does not exist: %s', nflowFile);
      end
      model = jsondecode(fileread(nflowFile));
      getBlock = @(k) NFlow.cellOrArrayElement(model.blocks, k);
      nBlocks = numel(model.blocks);

      staging = tempname();
      mkdir(staging);
      resDir = fullfile(staging, 'resources');
      mkdir(resDir);
      esc = @(s) strrep(strrep(strrep(strrep(char(s), '&', '&amp;'), '<', '&lt;'), '>', '&gt;'), '"', '&quot;');

      % Index the FMU blocks: component name, ordered input/output connector
      % names (native inspection), and the To Workspace sinks (system outputs).
      compIndex = struct('id', {}, 'name', {}, 'inputs', {}, 'outputs', {});
      sinkVar = containers.Map('KeyType', 'char', 'ValueType', 'char');
      body = '';
      for k = 1:nBlocks
        blk = getBlock(k);
        btype = char(blk.type);
        if strcmp(btype, 'fmu')
          fmuPath = char(blk.params.path);
          if ~isfile(fmuPath)
            error('Nelson:nflow_engine:fmuNotFound', 'FMU block source not found: %s', fmuPath);
          end
          cname = regexprep(char(blk.id), '[^a-zA-Z0-9_]', '_');
          target = fullfile(resDir, [cname, '.fmu']);
          copyfile(fmuPath, target);
          [inNames, outNames, connectors] = NFlow.fmuConnectorList(target, esc);
          % Carry the block's parameter overrides back out as a .ssv bound to the
          % component, so a composition imported with parameters re-exports them
          % (SSV round-trip). Only structs with fields produce a binding.
          paramBinding = '';
          if isfield(blk, 'params') && isfield(blk.params, 'parameterValues') ...
              && isstruct(blk.params.parameterValues) ...
              && ~isempty(fieldnames(blk.params.parameterValues))
            ssvName = [cname, '.ssv'];
            NFlow.writeSsv(fullfile(resDir, ssvName), blk.params.parameterValues, cname);
            paramBinding = ['        <ssd:ParameterBindings>', char(10), ...
              '          <ssd:ParameterBinding source="resources/', esc(ssvName), '"/>', char(10), ...
              '        </ssd:ParameterBindings>', char(10)];
          end
          body = [body, ...
            '      <ssd:Component name="', esc(cname), '" source="resources/', esc(cname), '.fmu"', ...
            ' type="application/x-fmu-sharedlibrary" implementation="CoSimulation">', char(10), ...
            '        <ssd:Connectors>', char(10), connectors, '        </ssd:Connectors>', char(10), ...
            paramBinding, ...
            '      </ssd:Component>', char(10)]; %#ok<AGROW>
          compIndex(end + 1) = struct('id', char(blk.id), 'name', cname, ...
            'inputs', {inNames}, 'outputs', {outNames}); %#ok<AGROW>
        elseif strcmp(btype, 'toWorkspace')
          vn = 'out';
          if isfield(blk, 'params') && isfield(blk.params, 'VariableName')
            vn = char(blk.params.VariableName);
          end
          sinkVar(char(blk.id)) = vn;
        end
      end
      findComp = @(bid) find(strcmp({compIndex.id}, bid), 1);

      % Connections: FMU->FMU wires become Connections; FMU->To Workspace wires
      % become system outputs. Everything else is dropped (system inputs have no
      % driver, exactly as sspImport leaves them).
      connXml = '';
      sysConn = struct('name', {}, 'kind', {});
      for k = 1:numel(model.connections)
        cn = NFlow.cellOrArrayElement(model.connections, k);
        fromId = char(cn.from); toId = char(cn.to);
        si = findComp(fromId);
        if isempty(si)
          continue; % only FMU outputs are exported as sources
        end
        fromIdx = double(cn.fromIndex) + 1;
        if fromIdx < 1 || fromIdx > numel(compIndex(si).outputs)
          continue;
        end
        startConn = compIndex(si).outputs{fromIdx};
        di = findComp(toId);
        if ~isempty(di)
          toIdx = double(cn.toIndex) + 1;
          if toIdx < 1 || toIdx > numel(compIndex(di).inputs)
            continue;
          end
          connXml = [connXml, ...
            '      <ssd:Connection startElement="', esc(compIndex(si).name), ...
            '" startConnector="', esc(startConn), ...
            '" endElement="', esc(compIndex(di).name), ...
            '" endConnector="', esc(compIndex(di).inputs{toIdx}), '"/>', char(10)]; %#ok<AGROW>
        elseif isKey(sinkVar, toId)
          outName = regexprep(sinkVar(toId), '[^a-zA-Z0-9_]', '_');
          sysConn = NFlow.addConnector(sysConn, outName, 'output');
          connXml = [connXml, ...
            '      <ssd:Connection startElement="', esc(compIndex(si).name), ...
            '" startConnector="', esc(startConn), ...
            '" endElement="" endConnector="', esc(outName), '"/>', char(10)]; %#ok<AGROW>
        end
      end

      sysConnXml = '';
      for i = 1:numel(sysConn)
        sysConnXml = [sysConnXml, ...
          '      <ssd:Connector name="', esc(sysConn(i).name), '" kind="', sysConn(i).kind, '">', char(10), ...
          '        <ssc:Real/>', char(10), ...
          '      </ssd:Connector>', char(10)]; %#ok<AGROW>
      end
      sysConnBlock = '';
      if ~isempty(sysConnXml)
        sysConnBlock = ['    <ssd:Connectors>', char(10), sysConnXml, '    </ssd:Connectors>', char(10)];
      end

      [~, sysName] = fileparts(sspFile);
      ssd = ['<?xml version="1.0" encoding="UTF-8"?>', char(10), ...
        '<ssd:SystemStructureDescription version="1.0" name="', esc(sysName), '"', ...
        ' xmlns:ssd="http://ssp-standard.org/SSP1/SystemStructureDescription"', ...
        ' xmlns:ssc="http://ssp-standard.org/SSP1/SystemStructureCommon">', char(10), ...
        '  <ssd:System name="', esc(sysName), '">', char(10), ...
        sysConnBlock, ...
        '    <ssd:Elements>', char(10), body, '    </ssd:Elements>', char(10), ...
        '    <ssd:Connections>', char(10), connXml, '    </ssd:Connections>', char(10), ...
        '  </ssd:System>', char(10), ...
        '</ssd:SystemStructureDescription>', char(10)];
      fid = fopen(fullfile(staging, 'SystemStructure.ssd'), 'wt');
      fwrite(fid, ssd);
      fclose(fid);

      if isfile(sspFile)
        delete(sspFile);
      end
      zip(sspFile, {fullfile(staging, 'SystemStructure.ssd'), resDir}, [staging, filesep]);
      sspPath = sspFile;
    end

    function [inNames, outNames, connectors] = fmuConnectorList(fmuPath, esc)
      % FMUCONNECTORLIST  Ordered input/output connector names + <ssd:Connector>
      % XML for an FMU, via native inspection. Shared by the SSP exporters.
      inNames = {}; outNames = {}; connectors = '';
      j = jsondecode(__nflow_fmi_inspect_json__(char(fmuPath)));
      if ~isfield(j, 'variables')
        return;
      end
      vars = j.variables;
      for v = 1:numel(vars)
        if iscell(vars)
          var = vars{v};
        else
          var = vars(v);
        end
        if strcmp(var.causality, 'input') || strcmp(var.causality, 'output')
          if strcmp(var.causality, 'input')
            inNames{end + 1} = var.name; %#ok<AGROW>
          else
            outNames{end + 1} = var.name; %#ok<AGROW>
          end
          connectors = [connectors, ...
            '        <ssd:Connector name="', esc(var.name), '" kind="', var.causality, '">', char(10), ...
            '          <ssc:Real/>', char(10), ...
            '        </ssd:Connector>', char(10)]; %#ok<AGROW>
        end
      end
    end

    function el = cellOrArrayElement(container, k)
      % CELLORARRAYELEMENT  Element k of a jsondecode result that may be a cell
      % array (heterogeneous structs) or a struct array (homogeneous).
      if iscell(container)
        el = container{k};
      else
        el = container(k);
      end
    end

    function bindMap = componentBindings(ssdText)
      % COMPONENTBINDINGS  Component -> parameter bindings, from .ssd text.
      %   Returns a struct array with fields 'name', 'sources' (a cell of bound
      %   .ssv paths) and 'inlineSets' (a cell of ParameterSet text blobs written
      %   straight into the .ssd). Each artefact is attributed to the nearest
      %   Component element opening before it; those preceding every Component
      %   (system-level) are ignored. Robust to the ssd:/ssc: namespace prefixes.
      bindMap = struct('name', {}, 'sources', {}, 'inlineSets', {});
      [compNames, compAt] = regexp(ssdText, ...
        '<\w*:?Component\b[^>]*\bname="([^"]*)"', 'tokens', 'start');
      if isempty(compNames)
        return;
      end
      compAt = double(compAt);
      names = cellfun(@(t) t{1}, compNames, 'UniformOutput', false);
      bindMap = struct('name', names, 'sources', repmat({{}}, 1, numel(names)), ...
        'inlineSets', repmat({{}}, 1, numel(names)));
      % Referenced .ssv files.
      [bindSrc, bindAt] = regexp(ssdText, ...
        '<\w*:?ParameterBinding\b[^>]*\bsource="([^"]*)"', 'tokens', 'start');
      for b = 1:numel(bindSrc)
        owner = find(compAt < bindAt(b), 1, 'last');
        if ~isempty(owner)
          bindMap(owner).sources{end + 1} = bindSrc{b}{1};
        end
      end
      % Inline ParameterSet blocks (values embedded directly in the .ssd).
      [setTok, setAt] = regexp(ssdText, ...
        '<\w*:?ParameterSet\b[\s\S]*?</\w*:?ParameterSet>', 'match', 'start');
      for s = 1:numel(setTok)
        owner = find(compAt < setAt(s), 1, 'last');
        if ~isempty(owner)
          bindMap(owner).inlineSets{end + 1} = setTok{s};
        end
      end
    end

    function pv = readSsv(ssvPath)
      % READSSV  Parameter name -> value map from an SSV file.
      %   pv = NFlow.readSsv(ssvPath) parses a System Structure Parameter Values
      %   (.ssv) file and returns a struct mapping each parameter name to its
      %   value. See NFlow.readSsvText for the value conventions.
      pv = NFlow.readSsvText(fileread(ssvPath));
    end

    function pv = readSsvText(txt)
      % READSSVTEXT  Parameter name -> value map from ParameterSet XML text.
      %   Parses <ssv:Parameter> entries out of a ParameterSet blob (an .ssv body
      %   or a ParameterSet embedded inline in a .ssd). Real/Integer/Enumeration
      %   become doubles; Boolean becomes logical. Names are sanitized to valid
      %   field names. Text-based because this DOM offers no scoped element query
      %   to pair a Parameter with its typed value child.
      pv = struct();
      m = regexp(txt, ...
        ['<\w*:?Parameter\b[^>]*\bname="([^"]*)"[^>]*>[\s\S]*?', ...
         '<\w*:?(Real|Integer|Enumeration|Boolean)\b[^>]*\bvalue="([^"]*)"'], ...
        'tokens');
      for i = 1:numel(m)
        nm = m{i}{1};
        kind = m{i}{2};
        raw = m{i}{3};
        if isempty(nm)
          continue;
        end
        fld = regexprep(nm, '[^a-zA-Z0-9_]', '_');
        if strcmp(kind, 'Boolean')
          pv.(fld) = strcmp(raw, 'true') || strcmp(raw, '1');
        else
          pv.(fld) = str2double(raw);
        end
      end
    end

    function writeSsv(ssvPath, pv, setName)
      % WRITESSV  Write a parameter name -> value struct as an SSV file.
      %   NFlow.writeSsv(ssvPath, pv) writes a System Structure Parameter Values
      %   (.ssv) file: each field of pv becomes an <ssv:Parameter>. The inverse of
      %   NFlow.readSsv. logical -> Boolean, integer -> Integer, everything else
      %   -> Real. Round-trips through readSsv.
      if nargin < 3 || isempty(setName)
        setName = 'parameters';
      end
      esc = @(s) strrep(strrep(strrep(strrep(char(s), '&', '&amp;'), '<', '&lt;'), '>', '&gt;'), '"', '&quot;');
      f = fieldnames(pv);
      body = '';
      for i = 1:numel(f)
        v = pv.(f{i});
        if islogical(v)
          if v
            val = 'true';
          else
            val = 'false';
          end
          typed = ['<ssv:Boolean value="', val, '"/>'];
        elseif isinteger(v)
          typed = ['<ssv:Integer value="', sprintf('%d', v), '"/>'];
        else
          typed = ['<ssv:Real value="', sprintf('%.15g', double(v)), '"/>'];
        end
        body = [body, ...
          '    <ssv:Parameter name="', esc(f{i}), '">', typed, '</ssv:Parameter>', char(10)]; %#ok<AGROW>
      end
      ssv = ['<?xml version="1.0" encoding="UTF-8"?>', char(10), ...
        '<ssv:ParameterSet version="1.0" name="', esc(setName), '"', ...
        ' xmlns:ssv="http://ssp-standard.org/SSP1/SystemStructureParameterValues"', ...
        ' xmlns:ssc="http://ssp-standard.org/SSP1/SystemStructureCommon">', char(10), ...
        '  <ssv:Parameters>', char(10), body, '  </ssv:Parameters>', char(10), ...
        '</ssv:ParameterSet>', char(10)];
      fid = fopen(char(ssvPath), 'wt');
      fwrite(fid, ssv);
      fclose(fid);
    end

    function s = mergeStruct(s, t)
      % MERGESTRUCT  Copy every field of t into s (t wins on conflicts).
      f = fieldnames(t);
      for i = 1:numel(f)
        s.(f{i}) = t.(f{i});
      end
    end

    function c = addConnector(c, name, kind)
      % ADDCONNECTOR  Append {name, kind} to a connector struct array, unique by
      % name (the first kind seen wins).
      if isempty(name) || any(strcmp({c.name}, name))
        return;
      end
      c(end + 1) = struct('name', name, 'kind', kind);
    end

    function sc = systemConnectors(ssdText)
      % SYSTEMCONNECTORS  Parent-system connectors from .ssd text.
      %   Returns a struct array {name, kind}. A Connector is system-level when it
      %   opens before the first Component element (component connectors live
      %   inside their Component); text-based because this DOM cannot scope a
      %   query to the System element.
      sc = struct('name', {}, 'kind', {});
      [~, compAt] = regexp(ssdText, '<\w*:?Component\b', 'match', 'start');
      firstComp = Inf;
      if ~isempty(compAt)
        firstComp = double(compAt(1));
      end
      [ctok, cat] = regexp(ssdText, '<\w*:?Connector\b[^>]*>', 'match', 'start');
      for i = 1:numel(ctok)
        if double(cat(i)) >= firstComp
          continue;
        end
        nm = regexp(ctok{i}, '\bname="([^"]*)"', 'tokens', 'once');
        kd = regexp(ctok{i}, '\bkind="([^"]*)"', 'tokens', 'once');
        if ~isempty(nm) && ~isempty(kd)
          sc = NFlow.addConnector(sc, nm{1}, kd{1});
        end
      end
    end

    function ver = fmuFmiVersion(fmuPath)
      % FMUFMIVERSION  FMI version string declared by an FMU's modelDescription.
      %   ver = NFlow.fmuFmiVersion(fmuPath) reads the fmiVersion attribute
      %   from the FMU archive's modelDescription.xml (e.g. '2.0', '3.0').
      %   Returns '' when it cannot be determined. Used to report a precise
      %   diagnostic for components the FMI 3.0 runtime cannot load.
      ver = '';
      tmp = [tempdir(), filesep, 'nflow_fmiver_', ...
        regexprep(tempname('.'), '[^a-zA-Z0-9_]', '_')];
      try
        mkdir(tmp);
        unzip(fmuPath, tmp);
        md = fullfile(tmp, 'modelDescription.xml');
        if isfile(md)
          txt = fileread(md);
          tok = regexp(txt, 'fmiVersion\s*=\s*"([^"]*)"', 'tokens', 'once');
          if ~isempty(tok)
            ver = tok{1};
          end
        end
      catch
      end
      if isdir(tmp)
        rmdir(tmp, 's');
      end
    end
  end
end
%=============================================================================
