%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef InstallerOptions
  properties
    ApplicationName = ''
    AdditionalFiles = {}
    AddRemoveProgramsIcon = ''
    AuthorCompany = ''
    AuthorEmail = ''
    AuthorName = ''
    Compression = 'normal'
    DefaultInstallationDir = ''
    Description = ''
    InstallationNotes = ''
    InstallerName = 'MyAppInstaller'
    InstallerIcon = ''
    InstallerLogo = ''
    OptionalDependencies = 'all'
    OutputDir = ''
    PackageType = 'auto'
    RuntimeDelivery = 'web'
    Shortcut = ''
    Summary = ''
    Version = '1.0'
    Verbose = false
  end
  methods
    function obj = InstallerOptions(varargin)
      narginchk(1, Inf);
      if isa(varargin{1}, 'compiler.build.Results')
        result = varargin{1};
        if ~isscalar(result)
          error('Nelson:nelsonc:options:installerOptionsRequireBuildResult', 'Installer options require one build result.');
        end
        obj.ApplicationName = result.Options.ExecutableName;
        obj.Shortcut = result.Files{1};
        varargin(1) = [];
      end
      pairs = __nelsonc_option_pairs__(properties(obj), varargin, false);
      for k = 1:2:numel(pairs)
        obj.(pairs{k}) = pairs{k + 1};
      end
      if isempty(obj.ApplicationName)
        error('Nelson:nelsonc:options:applicationNameRequired', 'ApplicationName is required.');
      end
      if isempty(obj.OutputDir)
        obj.OutputDir = [obj.ApplicationName, 'installer'];
      end
      if isempty(obj.DefaultInstallationDir)
        if ispc()
          obj.DefaultInstallationDir = ['%ProgramFiles%/', obj.ApplicationName];
        elseif ismac()
          obj.DefaultInstallationDir = ['/Applications/', obj.ApplicationName];
        else
          obj.DefaultInstallationDir = ['/usr/', obj.ApplicationName];
        end
      end
    end
    function obj = set.ApplicationName(obj, value)
      obj.ApplicationName = __nelsonc_installer_option__('ApplicationName', value);
    end
    function obj = set.AdditionalFiles(obj, value)
      obj.AdditionalFiles = __nelsonc_installer_option__('AdditionalFiles', value);
    end
    function obj = set.AuthorCompany(obj, value)
      obj.AuthorCompany = __nelsonc_installer_option__('AuthorCompany', value);
    end
    function obj = set.AddRemoveProgramsIcon(obj, value)
      obj.AddRemoveProgramsIcon = __nelsonc_installer_option__('AddRemoveProgramsIcon', value);
    end
    function obj = set.InstallerIcon(obj, value)
      obj.InstallerIcon = __nelsonc_installer_option__('InstallerIcon', value);
    end
    function obj = set.InstallerLogo(obj, value)
      obj.InstallerLogo = __nelsonc_installer_option__('InstallerLogo', value);
    end
    function obj = set.AuthorEmail(obj, value)
      obj.AuthorEmail = __nelsonc_installer_option__('AuthorEmail', value);
    end
    function obj = set.AuthorName(obj, value)
      obj.AuthorName = __nelsonc_installer_option__('AuthorName', value);
    end
    function obj = set.Compression(obj, value)
      obj.Compression = __nelsonc_installer_option__('Compression', value);
    end
    function obj = set.DefaultInstallationDir(obj, value)
      obj.DefaultInstallationDir = __nelsonc_installer_option__('DefaultInstallationDir', value);
    end
    function obj = set.Description(obj, value)
      obj.Description = __nelsonc_installer_option__('Description', value);
    end
    function obj = set.InstallationNotes(obj, value)
      obj.InstallationNotes = __nelsonc_installer_option__('InstallationNotes', value);
    end
    function obj = set.InstallerName(obj, value)
      obj.InstallerName = __nelsonc_installer_option__('InstallerName', value);
    end
    function obj = set.OptionalDependencies(obj, value)
      obj.OptionalDependencies = __nelsonc_installer_option__('OptionalDependencies', value);
    end
    function obj = set.OutputDir(obj, value)
      obj.OutputDir = __nelsonc_installer_option__('OutputDir', value);
    end
    function obj = set.PackageType(obj, value)
      obj.PackageType = __nelsonc_installer_option__('PackageType', value);
    end
    function obj = set.RuntimeDelivery(obj, value)
      obj.RuntimeDelivery = __nelsonc_installer_option__('RuntimeDelivery', value);
    end
    function obj = set.Shortcut(obj, value)
      obj.Shortcut = __nelsonc_installer_option__('Shortcut', value);
    end
    function obj = set.Summary(obj, value)
      obj.Summary = __nelsonc_installer_option__('Summary', value);
    end
    function obj = set.Version(obj, value)
      obj.Version = __nelsonc_installer_option__('Version', value);
    end
    function obj = set.Verbose(obj, value)
      obj.Verbose = __nelsonc_installer_option__('Verbose', value);
    end
    function values = struct(obj)
      if ~isscalar(obj)
        error('Nelson:nelsonc:options:installerOptionsScalar', 'Installer options must be scalar.');
      end
      values = struct();
      names = properties(obj);
      for k = 1:numel(names)
        values.(names{k}) = obj.(names{k});
      end
    end
  end
end
