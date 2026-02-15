DROP TABLE IF EXISTS subscriptions;
ALTER TABLE users DROP COLUMN IF EXISTS stripe_customer_id;
