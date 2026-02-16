-- Sample SQL file for parser testing

-- Create tables
CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    email VARCHAR(255) NOT NULL UNIQUE,
    name VARCHAR(100),
    created_at TIMESTAMP DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS orders (
    id SERIAL PRIMARY KEY,
    user_id INTEGER REFERENCES users(id),
    total DECIMAL(10,2) NOT NULL,
    status VARCHAR(20) DEFAULT 'pending',
    created_at TIMESTAMP DEFAULT NOW()
);

CREATE TABLE order_items (
    id SERIAL PRIMARY KEY,
    order_id INTEGER REFERENCES orders(id),
    product_name VARCHAR(200),
    quantity INTEGER NOT NULL,
    price DECIMAL(10,2) NOT NULL
);

-- Create views
CREATE VIEW active_users AS
SELECT id, email, name
FROM users
WHERE created_at > NOW() - INTERVAL '30 days';

CREATE OR REPLACE VIEW order_summary AS
SELECT o.id, u.name, o.total, o.status
FROM orders o
JOIN users u ON u.id = o.user_id;

-- Create functions
CREATE FUNCTION calculate_total(order_id INTEGER)
RETURNS DECIMAL AS $$
BEGIN
    RETURN (SELECT SUM(quantity * price) FROM order_items WHERE order_items.order_id = $1);
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION update_timestamp()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Create procedures
CREATE PROCEDURE archive_old_orders(cutoff_date DATE)
LANGUAGE plpgsql AS $$
BEGIN
    INSERT INTO archived_orders SELECT * FROM orders WHERE created_at < cutoff_date;
    DELETE FROM orders WHERE created_at < cutoff_date;
END;
$$;

-- Create triggers
CREATE TRIGGER users_updated_at
BEFORE UPDATE ON users
FOR EACH ROW
EXECUTE FUNCTION update_timestamp();

CREATE TRIGGER orders_audit
AFTER INSERT OR UPDATE ON orders
FOR EACH ROW
EXECUTE FUNCTION log_order_change();

-- Create indexes
CREATE INDEX idx_users_email ON users(email);
CREATE UNIQUE INDEX idx_orders_user_status ON orders(user_id, status);

-- Insert some data (should not be detected as symbols)
INSERT INTO users (email, name) VALUES ('alice@example.com', 'Alice');
SELECT * FROM users WHERE id = 1;
UPDATE users SET name = 'Bob' WHERE id = 2;
