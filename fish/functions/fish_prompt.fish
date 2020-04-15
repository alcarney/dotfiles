function fish_prompt
    echo  'λ '
end

function fish_right_prompt
    set -g __fish_git_prompt_show_informative_status

    #echo -n (set_color white) (fish_git_prompt) (set_color normal)
    echo -n (set_color -i aaa) (prompt_pwd) (set_color normal)
end
