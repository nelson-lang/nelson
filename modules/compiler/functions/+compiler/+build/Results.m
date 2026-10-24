%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef Results
  properties (SetAccess = private)
    BuildType = ''
    Files = {}
    IncludedSupportPackages = {}
    Options = []
    RuntimeDependencies = []
  end
  properties (Access = private)
    Report = struct()
  end
  methods
    function obj = Results(report, options, files, packages)
      narginchk(4, 4);
      if ~isstruct(report) || ~isscalar(report) ...
        || ~all(isfield(report, {'BuildType', 'RuntimePlan', 'Executable', 'SHA256'})) ...
        || ~isa(options, 'compiler.build.StandaloneApplicationOptions') || ~isscalar(options) ...
        || ~iscellstr(files) || ~isvector(files) ...
        || ~iscellstr(packages)
        error('Nelson:nelsonc:result', 'Invalid standalone build result.');
      end
      obj.BuildType = report.BuildType;
      obj.Files = files(:);
      obj.IncludedSupportPackages = packages;
      obj.Options = options;
      obj.RuntimeDependencies = compiler.runtime.Dependencies(report.RuntimePlan);
      obj.Report = report;
    end
    function report = __nelsonc_report__(obj)
      report = obj.Report;
    end
  end
end
