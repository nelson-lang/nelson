%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function check_xmldoc()
    disp(_('Checking XML documentation...'));
    modules_path = [nelsonroot(), '/modules/'];
    module_list = dir(modules_path);
    for i = 1:length(module_list)
        if module_list(i).isdir && ~ismember(module_list(i).name, {'.', '..'})
            module_name = module_list(i).name;
            disp(['Checking module: ''', module_name, '''...']);
            for lang = getavailablelanguages()'
               help_path = [modules_path, module_name, '/help/', lang{1}, '/xml/'];
               if isdir(help_path)
                   xmlfiles = dir([help_path, '*.xml']);
                   for j = 1:length(xmlfiles)
                       xmlfile = [help_path, xmlfiles(j).name];
                       [status, msg] = xmldocchecker(xmlfile);
                       if ~status
                        disp(['  Checking file: ', xmlfile]);                        
                        msg = msg{1};
                        msg = strrep(msg, '\n', sprintf('\n'));
                        msg = sprintf('    Error: %s\n', msg);
                        disp(msg);
                        return;
                       end
                   end
               end
            end
        end
    end
end
%=============================================================================