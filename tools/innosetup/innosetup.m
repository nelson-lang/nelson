%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
[p, f, e] = fileparts(nfilename('fullpathext'));
generated_filename = [p, '/', 'generated.iss'];
fp = fopen(generated_filename, 'wt');
if fp ~= -1
  fprintf(fp, ['#define GENERATED_INFO', char(10)]);
  compiler = version('-compiler');
  vernumber = version('-number');
  isQt5 = isfile([modulepath('nelson', 'builtin'), '/', 'Qt5Core.dll']);
  isQt6 = isfile([modulepath('nelson','builtin'), '/', 'Qt6Core.dll']);
  if isQt5
    fprintf(fp, ['#define QT5_USED', char(10)]);
  end
  if isQt6
    fprintf(fp, ['#define QT6_USED', char(10)]);
  end
  if strcmp(compiler{1}, 'icx') == true
    fprintf(fp, ['#define ICX_COMPILER', char(10)]);
  end
  if strcmp(compiler{1}, 'icc') == true
    fprintf(fp, ['#define ICC_COMPILER', char(10)]);
  end
  if strcmp(compiler{1}, 'msvc') == true
    fprintf(fp, ['#define VS_COMPILER', char(10)]);
  end
  if strcmp(compiler{3}, '64') == true
    fprintf(fp, ['#define NELSON_X64', char(10)]);
    fprintf(fp, ['#define WITH_JULIA_ENGINE', char(10)]);
  end
  if strcmp(compiler{2}, 'debug') == true
    fprintf(fp, ['#define NELSON_DEBUG', char(10)]);
  end
  fprintf(fp, ['#define APPLICATION_VERSION "', int2str(vernumber(1)), '.', int2str(vernumber(2)), '.', int2str(vernumber(3)), '.', int2str(vernumber(4)), '"', char(10)]);
  currentdate = datevec(now);
  fprintf(fp, ['#define CURRENT_YEAR "', int2str(currentdate(1)), '"', char(10)]);
  fclose(fp);
end
%=============================================================================
