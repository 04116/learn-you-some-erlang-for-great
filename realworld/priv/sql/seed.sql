-- Seed data for RealWorld application
-- This file contains sample data for testing

-- Insert sample users (passwords are hashed version of "password123")
INSERT INTO users (id, email, username, password_hash, bio, image) VALUES 
(
    'a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11',
    'john@example.com',
    'johndoe',
    '$2b$12$LQv3c1yqBWVHxkd0LHAkCOYz6TtxMQJqhN8/LewdBPj3A5Qjy.OJy',
    'I love writing about technology and life.',
    'https://via.placeholder.com/150'
),
(
    'b1ffcd88-8c1a-5df9-ac7e-7cc8ce481b22',
    'jane@example.com',
    'janesmith',
    '$2b$12$LQv3c1yqBWVHxkd0LHAkCOYz6TtxMQJqhN8/LewdBPj3A5Qjy.OJy',
    'Software engineer passionate about functional programming.',
    'https://via.placeholder.com/150'
),
(
    'c2ddfe77-7b2b-6ee0-bd8f-8dd9df572c33',
    'bob@example.com',
    'bobwilson',
    '$2b$12$LQv3c1yqBWVHxkd0LHAkCOYz6TtxMQJqhN8/LewdBPj3A5Qjy.OJy',
    'DevOps engineer who loves automation.',
    null
) ON CONFLICT (email) DO NOTHING;

-- Note: The password for all test users is "password123"
-- You can use these credentials to test the login functionality 