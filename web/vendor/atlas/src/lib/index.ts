/**
 * Atlas Design System
 *
 * A comprehensive design system for Svelte 5 with Tailwind v4.
 * Provides design tokens, utility functions, and reusable components.
 */

// Utilities
export { cn } from './utils/cn';

// Tokens
export * from './tokens';

// Components
export {
	// Core
	Button,
	Card,
	Badge,
	Spinner,
	Alert,
	Avatar,
	Tooltip,
	Skeleton,
	// Form
	Input,
	Textarea,
	Select,
	Checkbox,
	Radio,
	RadioGroup,
	Switch,
	Slider,
	// Layout
	PageHeader,
	EmptyState,
	StatCard,
	Divider,
	Tabs,
	Accordion,
	// Overlay
	Modal,
	Drawer,
	Dropdown,
	Popover,
	Toast,
	toast,
	// Navigation
	Breadcrumbs,
	Pagination,
	Navbar,
	Sidebar,
	// Data
	Table
} from './components';

// Layout Patterns
export {
	DashboardLayout,
	AuthLayout,
	SettingsLayout,
	FormWizard
} from './patterns';

// Page Templates
export {
	PricingSection,
	EmptyDashboard,
	OnboardingFlow
} from './templates';
