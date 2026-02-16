#import <Foundation/Foundation.h>
#import <UIKit/UIKit.h>
#import "AppDelegate.h"
#import "ViewController.h"
@import CoreData;
@import MapKit;

#define APP_NAME @"SampleApp"
#define MAX_RETRIES 3

/* Block comment
 * @interface FakeClass : NSObject
 * should not be parsed
 */

@protocol DataSourceDelegate <NSObject>

- (NSInteger)numberOfItems;
- (NSString *)itemAtIndex:(NSInteger)index;

@optional
- (void)didSelectItem:(NSString *)item;

@end

@protocol Configurable

- (void)configure:(NSDictionary *)options;

@end

@interface NetworkManager : NSObject <DataSourceDelegate>

@property (nonatomic, strong) NSString *baseURL;
@property (nonatomic, assign) NSInteger timeout;
@property (nonatomic, readonly) BOOL isConnected;

+ (instancetype)sharedManager;
- (void)fetchDataWithCompletion:(void (^)(NSData *, NSError *))completion;
- (BOOL)postData:(NSData *)data toEndpoint:(NSString *)endpoint;

@end

@interface CacheManager : NSObject

@property (nonatomic, strong) NSMutableDictionary *cache;

- (void)setObject:(id)object forKey:(NSString *)key;
- (id)objectForKey:(NSString *)key;

@end

@implementation NetworkManager

+ (instancetype)sharedManager {
    static NetworkManager *manager = nil;
    static dispatch_once_t onceToken;
    dispatch_once(&onceToken, ^{
        manager = [[self alloc] init];
    });
    return manager;
}

- (void)fetchDataWithCompletion:(void (^)(NSData *, NSError *))completion {
    // Implementation
}

- (BOOL)postData:(NSData *)data toEndpoint:(NSString *)endpoint {
    return YES;
}

- (NSInteger)numberOfItems {
    return 0;
}

- (NSString *)itemAtIndex:(NSInteger)index {
    return @"";
}

@end

@implementation CacheManager

- (void)setObject:(id)object forKey:(NSString *)key {
    self.cache[key] = object;
}

- (id)objectForKey:(NSString *)key {
    return self.cache[key];
}

@end
