%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef StandaloneApplicationOptions
  properties
    AppFile = ''
    AdditionalFiles = {}
    AutoDetectDataFiles = true
    CustomHelpTextFile = ''
    EmbedArchive = true
    ExecutableName = ''
    ExecutableIcon = ''
    ExecutableVersion = '1.0.0.0'
    OutputDir = ''
    RuntimeLogFile = ''
    SupportPackages = {'autodetect'}
    TreatInputsAsNumeric = false
    Verbose = false
  end
  methods
    function obj = StandaloneApplicationOptions(entry, varargin)
      narginchk(1, Inf);
      obj.AppFile = entry;
      [parent, name] = fileparts(obj.AppFile);
      obj.ExecutableName = name;
      pairs = __nelsonc_option_pairs__(properties(obj), varargin, false);
      for k = 1:2:numel(pairs)
        obj.(pairs{k}) = pairs{k + 1};
      end
      if isempty(obj.OutputDir)
        obj.OutputDir = [obj.ExecutableName, 'standaloneApplication'];
      end
    end
    function obj = set.AppFile(obj, value)
      obj.AppFile = __nelsonc_option_value__('AppFile', value);
    end
    function obj = set.AdditionalFiles(obj, value)
      obj.AdditionalFiles = __nelsonc_option_value__('AdditionalFiles', value);
    end
    function obj = set.AutoDetectDataFiles(obj, value)
      obj.AutoDetectDataFiles = __nelsonc_option_value__('AutoDetectDataFiles', value);
    end
    function obj = set.CustomHelpTextFile(obj, value)
      obj.CustomHelpTextFile = __nelsonc_option_value__('CustomHelpTextFile', value);
    end
    function obj = set.EmbedArchive(obj, value)
      obj.EmbedArchive = __nelsonc_option_value__('EmbedArchive', value);
    end
    function obj = set.ExecutableName(obj, value)
      obj.ExecutableName = __nelsonc_option_value__('ExecutableName', value);
    end
    function obj = set.ExecutableIcon(obj, value)
      obj.ExecutableIcon = __nelsonc_option_value__('ExecutableIcon', value);
    end
    function obj = set.ExecutableVersion(obj, value)
      obj.ExecutableVersion = __nelsonc_option_value__('ExecutableVersion', value);
    end
    function obj = set.OutputDir(obj, value)
      obj.OutputDir = __nelsonc_option_value__('OutputDir', value);
    end
    function obj = set.RuntimeLogFile(obj, value)
      obj.RuntimeLogFile = __nelsonc_option_value__('RuntimeLogFile', value);
    end
    function obj = set.Verbose(obj, value)
      obj.Verbose = __nelsonc_option_value__('Verbose', value);
    end
    function obj = set.TreatInputsAsNumeric(obj, value)
      obj.TreatInputsAsNumeric = __nelsonc_option_value__('TreatInputsAsNumeric', value);
    end
    function obj = set.SupportPackages(obj, value)
      obj.SupportPackages = __nelsonc_option_value__('SupportPackages', value);
    end
    function values = struct(obj)
      if ~isscalar(obj)
        error('Nelson:nelsonc:options', 'Build options must be scalar.');
      end
      values = struct();
      names = properties(obj);
      for k = 1:numel(names)
        values.(names{k}) = obj.(names{k});
      end
    end
  end
end
