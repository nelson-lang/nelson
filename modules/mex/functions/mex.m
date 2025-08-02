%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function mex(varargin)
  if nargin() == 0
    error(_('Wrong number of input arguments.'));
  end
  params = varargin(:)';
  if nargin() > 1
    isengine = ischar(params{1}) && (strcmp(params{1}, '-client') == true) && ...
    ischar(params{2}) && (strcmp(params{2}, 'engine') == true);
  else
    isengine = false;
  end
  
  filenames = getFilenames(params);
  definesParams = getDefines(params);
  includesParams = getIncludes(params);
  librariesParams = getLibraries(params);
  undefinesParams = getUndefines(params);
  cflagsParams = getCFlags(params);
  buildConfigParam = getBuildConfig(params);
  [status, apiVersion] = containsApiVersion(params);
  hasinterleavedcomplex = false;
  interleavedComplex = '';
  nelsonIdentified = 'MX_IS_NELSON';
  if status
    if strcmp(apiVersion, '-R2018a') == true
      hasinterleavedcomplex = true;
      interleavedComplex = 'MX_HAS_INTERLEAVED_COMPLEX';
    end
  end
  [status, outputName] = containsOutput(params);
  if status
    functionName = outputName;
  else
    [p, f, e ] = fileparts(filenames{1});
    functionName = f;
  end
  if isempty(interleavedComplex)
    defines = {nelsonIdentified};
  else
    defines = { nelsonIdentified, interleavedComplex };
  end
  defines = [defines, definesParams];
  
  destinationPath = fileparts(filenames{1}, 'path');
  if isempty(destinationPath)
    destinationPath = pwd();
  end
  if ~ispc()
    c_flags = '-Werror=implicit-function-declaration';
    cxx_flags = '-Werror=implicit-function-declaration';
  else
    c_flags = '';
    cxx_flags = '';
  end
  
  includes = [{destinationPath}; includesParams'; getMexIncludes()];
  libraries = [librariesParams'; getMexLibraries()];
  c_flags = [concateToChar(c_flags), ' ', concateToChar(cflagsParams),  ' ', concateToChar(undefinesParams)];
  c_flags = strtrim(c_flags);
  build_configuration = buildConfigParam;
  
  if ~isengine
    dlgeneratemexgateway(destinationPath, functionName, hasinterleavedcomplex);
    filenames = [filenames, 'mexGateway.c'];
    maketype = 'dynamic_library';
  else
    maketype = 'executable';
  end
  [status, message, cmakefilename] = dlgeneratemake(maketype, destinationPath, ...
  functionName, ...
  filenames, ...
  includes, ...
  defines, ...
  libraries, ...
  build_configuration, ...
  c_flags, ...
  cxx_flags);
  if ~status
    error(message);
  end
  [status, message] = dlmake(destinationPath);
  if ~status
    error(message);
  end
  if ~isengine
    clear(functionName)
    if ~endsWith(destinationPath,'/') && ~endsWith(destinationPath,'\')
      destinationPath = [destinationPath, '/'];
    end
    mexGatewayFilename = [destinationPath, '/mexGateway.c'];
    rmfile(mexGatewayFilename)
    fullDestinationDynamicLibraryName =  [destinationPath, functionName, getdynlibext()];
    fullDestinationMexName = [destinationPath, functionName, '.', mexext()];
    copyfile(fullDestinationDynamicLibraryName, fullDestinationMexName)
    rmfile(fullDestinationDynamicLibraryName)
    rmfile(cmakefilename)
    if ispc()
      fullDestinationLibraryName =  [destinationPath, functionName, '.lib'];
      rmfile(fullDestinationLibraryName)
    end
  end
end
%=============================================================================
function [status, apiVersion] = containsApiVersion(params)
  status = false;
  apiVersion = '-R2017b';
  for v = params
    if ischar(v{1})
      s = strtrim(v{1});
      if strcmp(s, '-R2018a') || strcmp(s, '-R2017b')
        status = true;
        apiVersion = s;
        break;
      end
    end
  end
end
%=============================================================================
function [status, outputName] = containsOutput(params)
  status = false;
  outputName = '';
  p = 1;
  len = length(params);
  for p = 1:len
    v = params{p};
    if ischar(v)
      if strcmp(v, '-output') == true
        outputName = params{p + 1};
        status = true;
        break;
      end
    end
    p = p + 1;
  end
end
%=============================================================================
function filenames = getFilenames(params)
  filenames = {};
  for p = params
    if ischar(p{1}) && isfile(p{1})
      isSupportedExtensions = endsWith(p{1}, '.c', 'IgnoreCase', true) || ...
      endsWith(p{1}, '.cc', 'IgnoreCase', true) || ...
      endsWith(p{1}, '.cxx', 'IgnoreCase', true) || ...
      endsWith(p{1}, '.cpp', 'IgnoreCase', true);
      if isSupportedExtensions
        filenames{end + 1} = strtrim(strrep(p{1}, '\', '/'));
      end
    end
  end
end
%=============================================================================
function defines = getDefines(params)
  defines = {};
  for p = params
    if ischar(p{1})
      isDefine = startsWith(p{1}, '-D', 'IgnoreCase', false);
      if isDefine
        s = strtrim(p{1});
        defines{end + 1} = s(3:length(s));
      end
    end
  end
end
%=============================================================================
function includes = getIncludes(params)
  includes = {};
  for p = params
    if ischar(p{1})
      s = strtrim(p{1});
      isInclude = startsWith(s, '-I', 'IgnoreCase', false);
      if isInclude
        s = removeDoubleQuote(s(3:length(s)));
        includes{end + 1} = strrep(s, '\','/');
      end
    end
  end
end
%=============================================================================
function libraries = getLibraries(params)
  libraries = {};
  for p = params
    if ischar(p{1})
      s = strtrim(p{1});
      isLibrary = startsWith(s, '-l', 'IgnoreCase', false);
      if isLibrary
        s = removeDoubleQuote(s(3:length(s)));
        libraries{end + 1} = strrep(s, '\', '/');
      else
        s = removeDoubleQuote(s);
        if endsWith(s, getSupportedLibraryExtension())
          s = s(1:length(s) - length(getSupportedLibraryExtension()));
          libraries{end + 1} = strrep(s, '\', '/');
        end
      end
    end
  end
end
%=============================================================================
function undefines = getUndefines(params)
  undefines = {};
  [tf, compiler] = havecompiler();
  ismsvc = strcmp(compiler, 'msvc');
  for p = params
    if ischar(p{1})
      s = strtrim(p{1});
      isUndefines = startsWith(s, '-U', 'IgnoreCase', false);
      if isUndefines
        if ismsvc
          undefines{end + 1} = ['/', s(2:length(s))];
        else
          undefines{end + 1} = s;
        end
      end
    end
  end
end
%=============================================================================
function buildConfig = getBuildConfig(params)
  buildConfig = 'Release';
  for p = params
    if ischar(p{1})
      s = strtrim(p{1});
      isDebug = strcmp(s, '-g');
      if isDebug
        buildConfig = 'Debug';
      end
    end
  end
end
%=============================================================================
function cflags = getCFlags(params)
  cflags = {};
  CFLAGS_PREFIX = 'CFLAGS=';
  LEN_CFLAGS_PREFIX = length(CFLAGS_PREFIX);
  for p = params
    if ischar(p{1})
      s = strtrim(p{1});
      isCflags = startsWith(s, CFLAGS_PREFIX, 'IgnoreCase', false);
      if isCflags
        cflags{end + 1} = removeDoubleQuotes(LEN_CFLAGS_PREFIX:length(s));
      end
    end
  end
end
%=============================================================================
function str = concateToChar(ce, delimiter)
  if ~isvar('delimiter')
    delimiter = ' ';
  end
  str = '';
  if ischar(ce)
    str = ce;
  else
    first = true;
    for e = ce
      if first
        str = [str, e{1}];
        first = false;
      else
        str = [str, delimiter, e{1}];
      end
    end
  end
  str = strtrim(str);
end
%=============================================================================
function s = removeDoubleQuote(str)
  s = strtrim(str);
  if startsWith(s, '"') || startsWith(s, '''')
    s = s(2:length(s));
  end
  if endsWith(s, '"') || endsWith(s, '"')
    s = s(1:length(s) - 1);
  end
end
%=============================================================================
function ext = getSupportedLibraryExtension()
  if ispc()
    ext = '.lib';
  else
    if ismac()
      ext = '.dylib';
    else
      ext = '.so';
    end
  end
end
%=============================================================================
function includes = getMexIncludes()
  if isdir([modulepath('mex'), '/src/include'])
    includes = {[modulepath('mex'), '/src/include']};
  else
    if isdir([modulepath('nelson', 'builtin'), '/../../include/Nelson/mex'])
      includes = {fullpath([modulepath('nelson', 'builtin'), '/../../include/Nelson/mex'])};
    else
      includes = {};
    end
  end
end
%=============================================================================
function libraries = getMexLibraries()
  libraries = {[modulepath('nelson', 'builtin'), '/libnlsMex']};
end
%=============================================================================
