%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [status, message, generatedfilename] = dlgeneratemake(varargin)
  % dlgeneratemake('f:/demo', 'toto', {'f:/demo/toto.cpp'}, {'f:/demo/'}, {'NLSDYNAMIC_LINK_EXPORTS'}, {'msvcrt.lib'},'Debug')
  % dlgeneratemake(destinationdir, libname, c_cpp_files, includes, defines, external_libraries, build_configuration, c_flags, cxx_flags)
  % dlgeneratemake('f:/demo', 'toto', {'f:/demo/toto.cpp'}, {'f:/demo/'}, {'NLSDYNAMIC_LINK_EXPORTS'})
  
  generatedfilename = [];
  status = false;
  message = '';
  if nargin < 4
    error(_('At least 4 input arguments expected.'));
  end
  if nargin > 10
    error(_('Wrong number of output arguments.'));
  end
  
  maketype = 'dynamic_library';
  indexstart = 1;
  if strcmp(varargin{1}, 'executable') == true
    maketype = 'executable';
    indexstart = 2;
  end
  if strcmp(varargin{1}, 'dynamic_library') == true
    maketype = 'dynamic_library';
    indexstart = 2;
  end
  
  destinationdir = varargin{indexstart};
  libname = varargin{indexstart + 1};
  c_cpp_files = varargin{indexstart + 2};
  includes = varargin{indexstart + 3};
  if nargin > indexstart + 3
    defines = varargin{indexstart + 4};
  else
    defines = [];
  end
  if nargin > indexstart + 4
    external_libraries = varargin{indexstart + 5};
  else
    external_libraries = [];
  end
  build_configuration = [];
  if nargin > indexstart + 5
    build_configuration = varargin{indexstart + 6};
  end
  if isempty(build_configuration)
    ver_comp = version('-compiler');
    if strcmp(ver_comp{2}, 'debug')
      build_configuration = 'Debug';
    else
      build_configuration = 'Release';
    end
  end
  c_flags = [];
  if nargin > indexstart + 6
    c_flags = varargin{indexstart + 7};
  end
  if isempty(c_flags)
    c_flags = '';
  end
  cxx_flags = [];
  if nargin > indexstart + 7
    cxx_flags = varargin{indexstart + 8};
  end
  if isempty(cxx_flags)
    cxx_flags = '';
  end
  
  r = checkDestinationDir(destinationdir);
  if ~r
    status = r;
    message = _('A valid destination directory expected.');
    return
  end
  r = checkLibraryName(libname);
  if ~r
    status = r;
    message = _('A valid destination library name expected.');
    return
  end
  [r, c_cpp_files] = checkAndPrepareCppFiles(c_cpp_files);
  if ~r
    status = r;
    message = _('A valid list of c/cpp files expected.');
    return
  end
  [r, includes] = checkAndPrepareIncludesDirectories(includes);
  if ~r
    status = r;
    message = _('A valid list of includes directories expected.');
    return
  end
  [r, defines] = checkAndPrepareDefines(defines);
  if ~r
    status = r;
    message = _('A valid list of define expected.');
    return
  end
  [r, external_libraries] = checkAndPrepareExternalLibraries(external_libraries);
  if ~r
    status = r;
    message = _('A valid list of external libraries expected.');
    return
  end
  [r, build_configuration] = checkAndPrepareBuildConfiguration(build_configuration);
  if ~r
    status = r;
    message = _('A valid build configuration value expected.');
    return
  end
  [r, c_flags] = checkAndPrepareCflags(c_flags);
  if ~r
    status = r;
    message = _('A valid c_flags configuration value expected.');
    return
  end
  [r, cxx_flags] = checkAndPrepareCflags(cxx_flags);
  if ~r
    status = r;
    message = _('A valid cxx_flags configuration value expected.');
    return
  end
  if strcmp(maketype, 'executable') == true
    templatefilename = [modulepath('dynamic_link'), '/resources/template_executable_cmake.txt'];
  else
    templatefilename = [modulepath('dynamic_link'), '/resources/template_cmake.txt'];
  end
  if ~isfile(templatefilename)
    error(_('template file is missing.'));
  end
  template_cmake = fileread(templatefilename);
  template_cmake = replace(template_cmake, '__OUTPUT_DIRECTORY__', destinationdir);
  template_cmake = replace(template_cmake, '__C_FLAGS__', c_flags);
  template_cmake = replace(template_cmake, '__CXX_FLAGS__', cxx_flags);
  template_cmake = replace(template_cmake, '__DEFINES__', defines);
  template_cmake = replace(template_cmake, '__C_CPP_FILES__', c_cpp_files);
  template_cmake = replace(template_cmake, '__INCLUDES__', includes);
  template_cmake = replace(template_cmake, '__EXTERNAL_LIBRARIES__', external_libraries);
  template_cmake = replace(template_cmake, '__CMAKE_BUILD_TYPE__', build_configuration);
  if strcmp(maketype, 'executable') == true
    template_cmake = replace(template_cmake, '__EXECUTABLE_NAME__', libname);
  else
    template_cmake = replace(template_cmake, '__LIBRARY_NAME__', libname);
  end
  template_cmake = replace(template_cmake, [newline, newline], newline);
  generatedfilename = [destinationdir, '/CMakeLists.txt'];
  filewrite(generatedfilename, template_cmake);
  status = true;
end
%=============================================================================
function r = checkDestinationDir(destinationdir)
  if ischar(destinationdir)
    r = isdir(destinationdir);
  else
    r = false;
  end
end
%=============================================================================
function r = checkLibraryName(libname)
  if ischar(libname)
    r = true;
  else
    r = false;
  end
end
%=============================================================================
function [r, c_cpp_files_modified] = checkAndPrepareCppFiles(c_cpp_files)
  c_cpp_files_modified = '';
  r = false;
  if ischar(c_cpp_files)
    c_cpp_files = {uniformizePath(c_cpp_files)};
  end
  if isempty(c_cpp_files)
    c_cpp_files = {};
  end
  if (isempty(c_cpp_files) && iscell(c_cpp_files))
    r = true;
    return
  end
  if iscellstr(c_cpp_files)
    for k = c_cpp_files(:)'
      if ~isfile(k{1})
        c_cpp_files_modified = '';
        r = false;
        return
      end
      c_cpp_files_modified = [c_cpp_files_modified, newline, ['"', uniformizePath(k{1}), '"']];
    end
    r = true;
  else
    r = false;
  end
end
%=============================================================================
function [r, includes_modified] = checkAndPrepareIncludesDirectories(includes)
  includes_modified = '';
  r = false;
  if ischar(includes)
    includes = {uniformizePath(includes)};
  end
  if isempty(includes)
    includes = {};
  end
  if (isempty(includes) && iscell(includes))
    r = true;
    return
  end
  if iscellstr(includes)
    for k = includes(:)'
      if ~isdir(k{1})
        includes_modified = '';
        r = false;
        return
      end
      includes_modified = [includes_modified, newline, ['"', uniformizePath(k{1}), '"']];
    end
    r = true;
  else
    r = false;
  end
end
%=============================================================================
function [r, defines_modified] = checkAndPrepareDefines(defines)
  defines_modified = '';
  r = false;
  if ischar(defines)
    defines = {defines};
  end
  if isempty(defines)
    defines = {};
  end
  if (isempty(defines) && iscell(defines))
    r = true;
    return
  end
  if iscellstr(defines)
    for k = defines(:)'
      if ~ischar(k{1})
        defines_modified = '';
        r = false;
        return
      end
      defines_modified = [defines_modified, newline, ['add_compile_definitions(', k{1}, ')']];
    end
    r = true;
  else
    r = false;
  end
end
%=============================================================================
function modifiedName = addLibrarySuffix(libraryName)
  modifiedName = libraryName;
  if ispc()
    if ~(endsWith(libraryName, '.a') || endsWith(libraryName, '.lib'))
      modifiedName = [libraryName, '.lib'];
    end
  else
    modifiedName = [libraryName, getdynlibext()];
  end
end
%=============================================================================
function [r, external_libraries_modified] = checkAndPrepareExternalLibraries(external_libraries)
  external_libraries_modified = '';
  r = false;
  if ischar(external_libraries)
    external_libraries = {uniformizePath(addLibrarySuffix(external_libraries))};
  end
  if isempty(external_libraries)
    external_libraries = {};
  end
  if (isempty(external_libraries) && iscell(external_libraries))
    r = true;
    return
  end
  if iscellstr(external_libraries)
    for k = external_libraries(:)'
      if ~ischar(k{1})
        external_libraries_modified = '';
        r = false;
        return
      end
      external_libraries_modified = [external_libraries_modified, newline, ['"', uniformizePath(addLibrarySuffix(k{1})), '"']];
    end
    r = true;
  else
    r = false;
  end
end
%=============================================================================
function [r, build_configuration_modified] = checkAndPrepareBuildConfiguration(build_configuration)
  build_configuration_modified = '';
  if isempty(build_configuration)
    ver_comp = version('-compiler');
    if strcmp(ver_comp{2}, 'debug')
      build_configuration = 'Debug';
    else
      build_configuration = 'Release';
    end
    r = true;
    return
  end
  if ischar(build_configuration)
    r = strcmp(build_configuration, 'Debug') || strcmp(build_configuration, 'Release');
    if r
      build_configuration_modified = ['set(CMAKE_BUILD_TYPE ', build_configuration, ')'];
    end
  else
    r = false;
  end
end
%=============================================================================
function [r, c_flags_modified] = checkAndPrepareCflags(c_flags)
  if isempty(c_flags)
    r = true;
    c_flags_modified = '';
  else
    if ischar(c_flags)
      r = true;
      c_flags_modified = ['set(CMAKE_C_FLAGS "', c_flags, '")'];
    else
      r = false;
      c_flags_modified = '';
    end
  end
end
%=============================================================================
function [r, cxx_flags_modified] = checkAndPrepareCppflags(cxx_flags)
  if isempty(cxx_flags)
    r = true;
    cxx_flags_modified = '';
  else
    if ischar(cxx_flags)
      r = true;
      cxx_flags_modified = ['set(CMAKE_CXX_FLAGS "', cxx_flags, '")'];
    else
      r = false;
      cxx_flags_modified = '';
    end
  end
end
%=============================================================================
function pathOut = uniformizePath(pathIn)
  pathOut = replace(pathIn, '\', '/');
end
%=============================================================================
