Facter.add(:user_home) do
    setcode do
        Dir.home($user)
    end
end

