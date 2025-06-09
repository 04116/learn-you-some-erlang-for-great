%% Header file for user record definition

-record(user, {
    id,
    email,
    username,
    password_hash,
    bio = null,
    image = null,
    created_at,
    updated_at
}). 