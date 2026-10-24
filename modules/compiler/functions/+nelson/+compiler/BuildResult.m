%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef BuildResult
  properties (SetAccess = private)
    BuildType = 'standaloneApplication'
    Files = {}
    Executable = ''
    RuntimeDirectory = ''
    Options = []
    DependencyPlan = struct()
    RuntimePlan = struct()
    Manifest = struct()
    SHA256 = ''
  end
  methods
    function obj = BuildResult(report, options)
      narginchk(2, 2);
      fields = {'Executable', 'RuntimeDirectory', 'DependencyPlan', 'RuntimePlan', 'Manifest', 'SHA256'};
      if ~isstruct(report) || ~isscalar(report) || ~all(isfield(report, fields)) ...
        || ~isa(options, 'nelson.compiler.BuildOptions') || ~isscalar(options)
        error('Nelson:nelsonc:result:invalidBuildReportOptions', 'Invalid build report or options.');
      end
      for k = 1:numel(fields)
        obj.(fields{k}) = report.(fields{k});
      end
      obj.Files = {report.Executable};
      if ~options.EmbedArchive
        [directory, name] = fileparts(report.Executable);
        obj.Files{end + 1} = fullfile(directory, [name, '.nca']);
      end
      if ~isempty(report.RuntimeDirectory)
        obj.Files{end + 1} = report.RuntimeDirectory;
      end
      obj.Options = options;
      if options.NoConsole
        obj.BuildType = 'standaloneWindowsApplication';
      end
    end
    function result = struct(obj)
      if ~isscalar(obj)
        error('Nelson:nelsonc:result:buildResultsScalar', 'Build results must be scalar.');
      end
      result = struct();
      names = properties(obj);
      for k = 1:numel(names)
        result.(names{k}) = obj.(names{k});
      end
      result.Options = struct(obj.Options);
    end
  end
end
