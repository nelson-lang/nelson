%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function xmldocrenderimages(varargin)
  % xmldocrenderimages() build images for Nelson help files
  % xmldocrenderimages(module_name) build images for a module loaded in Nelson
  %
  narginchk(0, 1);
   currentLang = getlanguage();

   if (nargin() == 0)
    modules_help_list = get_modules_help_list();
    for lang = getavailablelanguages()'
      setlanguage(lang{1});
      for m = modules_help_list'
        xmldocrenderimages_module(m{1}, lang{1});
      end
    end
    setlanguage(currentLang);
    return;
  end
  module = varargin{1};
  if ~ismodule(module)
    error('Invalid module name');
  end
  module_path = modulepath(module);
  for lang = getavailablelanguages()'
    xmldocrenderimages_module(module, lang{1});
  end
  setlanguage(currentLang);
end
%=============================================================================
function xmldocrenderimages_module(module, language)
  fprintf('Rendering images for module %s language %s\n', module, language);
  if ~ismodule(module)
    return;
  end
  module_path = modulepath(module);
  xml_module_path = fullfile(module_path, 'help', language, 'xml');
  if ~isdir(xml_module_path)
    return;
  end
  TMP = [tempname() '_xmldocrenderimages'];
  mkdir(TMP);
  [status, scriptFiles, msg] = __xmldocgenerateimages__(xml_module_path, TMP);
  if ~status
    error(msg);
  end
  nbWorkers = maxNumCompThreads();
  TIMEOUT = 60;
  accumulator = {};
  for i = 1:numel(scriptFiles)
    nelson_bin_path = modulepath('nelson', 'bin');
    nelson_exe_path = ['"', nelson_bin_path, '/', 'nelson-adv-cli.exe', '"'];
    cmd = [nelson_exe_path, ' --timeout ', num2str(TIMEOUT), ' --quiet --file ' , scriptFiles{i}];
    accumulator = [accumulator, cmd];
    if (length(accumulator) == nbWorkers || i == numel(scriptFiles))
        [r, msgerr] = system(accumulator, TIMEOUT - 5);
        accumulator = {};
        for j = 1:length(r)
          if (r(j) ~= 0)
            msg = sprintf(_('Error while rendering image: %s'), scriptFiles{i-(length(r)-j)});
            error(msg);
          end
        end
    end
  end
  rmdir(TMP, 's');
end
%=============================================================================
