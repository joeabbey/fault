package cloud

import (
	"context"
	"time"

	"github.com/joeabbey/magma/pkg/limits"
	"github.com/joeabbey/magma/pkg/models"
)

// Plan tier limits for LLM calls per month.
const (
	FreeLLMLimit = 50
	ProLLMLimit  = 1000
)

// LimitsStorageAdapter wraps the cloud Store to satisfy magma's limits.Storage interface.
// It reads from the existing usage table. IncrementUsage is a no-op because the
// analysis handlers already call store.IncrementUsage after each LLM call.
type LimitsStorageAdapter struct {
	store Store
}

// NewLimitsStorageAdapter creates a new adapter wrapping the given store.
func NewLimitsStorageAdapter(store Store) *LimitsStorageAdapter {
	return &LimitsStorageAdapter{store: store}
}

// GetUsage returns the current month's usage as a magma Usage struct.
func (a *LimitsStorageAdapter) GetUsage(ctx context.Context, entityID string) (*models.Usage, error) {
	month := time.Now().Format("2006-01")
	usage, err := a.store.GetUsage(ctx, entityID, month)
	if err != nil {
		return nil, err
	}
	return &models.Usage{
		EntityID: entityID,
		Period:   month,
		Counters: map[string]int{
			"llm_calls": usage.LLMCalls,
		},
	}, nil
}

// IncrementUsage is a no-op. The analysis handlers already track usage
// via store.IncrementUsage after each successful LLM call.
func (a *LimitsStorageAdapter) IncrementUsage(ctx context.Context, entityID, resource string) error {
	return nil
}

// FaultPlanProvider implements magma's limits.PlanProvider by looking up the
// user's plan from the store and mapping it to a SimplePlan with LLM call limits.
type FaultPlanProvider struct {
	store Store
}

// NewFaultPlanProvider creates a new plan provider wrapping the given store.
func NewFaultPlanProvider(store Store) *FaultPlanProvider {
	return &FaultPlanProvider{store: store}
}

// GetPlan returns the plan for the given user ID.
func (p *FaultPlanProvider) GetPlan(entityID string) limits.Plan {
	user, err := p.store.GetUserByID(context.Background(), entityID)
	if err != nil || user == nil {
		return planForName("free")
	}
	return planForName(user.Plan)
}

// planForName returns a SimplePlan for the given plan name.
func planForName(name string) limits.SimplePlan {
	switch name {
	case "pro":
		return limits.SimplePlan{
			PlanName:    "pro",
			Limits:      map[string]int{"llm_calls": ProLLMLimit},
			AutoApprove: true,
		}
	case "team":
		return limits.SimplePlan{
			PlanName:    "team",
			Limits:      map[string]int{"llm_calls": limits.Unlimited},
			AutoApprove: true,
		}
	default:
		return limits.SimplePlan{
			PlanName:    "free",
			Limits:      map[string]int{"llm_calls": FreeLLMLimit},
			AutoApprove: false,
		}
	}
}
