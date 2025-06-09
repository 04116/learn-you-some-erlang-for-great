%% Header file for article record definition

-record(article, {
    id,
    slug,
    title,
    description,
    body,
    author_id,
    created_at,
    updated_at
}). 